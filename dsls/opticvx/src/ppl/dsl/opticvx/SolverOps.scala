package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, PrimitiveOps, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait SolverOps extends Base {

}

trait SolverOpsExp extends SolverOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with OptVarOpsExp with ExprShapeOpsExp with StringOpsExp with WhileExp
    with MiscOpsExp with ConstraintOpsExp with VectorOpsExp with IfThenElseExp 
    with VariablesExp with MathOpsExp with AbstractMatrixOpsExp with PrimitiveOps =>
  
  case class SymmetricCone(
    //size of unconstrained variables
    val unconstrained_sz: Exp[Int], 
    //size of positive simplex variables
    val psimplex_sz: Exp[Int],
    //n values for second-order-cone constraints 
    val soc_ns: Seq[Exp[Int]],
    //n values for definitness constraints
    val definite_ns: Seq[Exp[Int]]
  ) {
    
  }
  
  //minimize c'*x subject to A*x + b = 0 and x \in K
  def solve(A: AbstractMatrix, b: Exp[CVXVector], c: Exp[CVXVector], K: SymmetricCone): Exp[CVXVector] = {
    println(Const("Matrix A is ") + string_valueof(A.m()) + Const(" by ") + string_valueof(A.n()))
    new Problem(A,b,c,K).solve()
  }
  
  class Problem(A: AbstractMatrix, b: Exp[CVXVector], c: Exp[CVXVector], K: SymmetricCone) {
    
    var Ainv_b: Exp[CVXVector] = null
    var PA: AbstractMatrix = null
    var c_hat: Exp[CVXVector] = null
    
    
    def solve(): Exp[CVXVector] = {
      println(Const("Setting up solver..."))
      setup()
      val x = var_new[CVXVector](vector_zeros(A.n()))
      val niters = var_new[Int](Const(0))
      println(Const("Solving..."))
      __whileDo(niters <= Const(100), {
        //print(Const("Iteration ") + string_valueof(readVar(niters)) + ": " + vector_to_string_matlab(readVar(x)))
        var_assign(x, vector_sum(readVar(x),vector_scale(c_hat,Const(-1.0)*step_size(niters))))
        //print(Const(" -> ") + vector_to_string_matlab(readVar(x)))
        var_assign(x, project_onto_K(readVar(x)))
        //println(Const(" -> ") + vector_to_string_matlab(readVar(x)))
        var_assign(x, project_onto_Axb(readVar(x)))
        var_assign(niters, readVar(niters)+Const(1))
      })
      x
    }
    
    def setup() {
      //compute A^-1*b
      //println(Const("         b = ") + vector_to_string_matlab(b))
      val Ainv = amatrix_inv_lsqr(A,Const(1e-20),Const(20))
      Ainv_b = Ainv.get_Ax(b)
      //println(Const("    Ainv*b = ") + vector_to_string_matlab(Ainv_b))
      //println(Const("A*(Ainv*b) = ") + vector_to_string_matlab(A.get_Ax(Ainv_b)))
      //setup the projection matrix
      val AATinv = amatrix_inv_lsqr(amatrix_prod(A,amatrix_transp(A)),Const(1e-20),Const(10))
      PA = amatrix_prod(amatrix_prod(amatrix_transp(A),AATinv),A)
      //normalize the objective
      c_hat = vector_scale(c, Const(1.0)/math_sqrt(vector_dot(c,c)))
    }
    
    def step_size(iter: Exp[Int]): Exp[Double] = {
      Const(0.01)/repIntToRepDouble(iter+Const(1))
    }
    
    def project_onto_Axb(x: Exp[CVXVector]): Exp[CVXVector] = {
      //xm = x - Ainv*b
      //println(Const("Projection call size: ") + string_valueof(vector_len(x)))
      //println(Const("Matrix PA is ") + string_valueof(PA.m()) + Const(" by ") + string_valueof(PA.n()))
      val xm = vector_sum(x,vector_scale(Ainv_b,Const(-1.0)))
      //
      val xmp = vector_sum(xm,vector_scale(PA.get_Ax(xm),Const(-1.0)))
      //
      val xrv = vector_sum(xmp,Ainv_b)
      //println(Const("Projection return size: ") + string_valueof(vector_len(xrv)))
      //return the computed value
      println(Const("in = ") + vector_to_string_matlab(x))
      println(Const(" b = ") + vector_to_string_matlab(b))
      println(Const("rv = ") + vector_to_string_matlab(xrv))
      println(Const("Am = ") + vector_to_string_matlab(A.get_Ax(xmp)))
      println(Const("Ar = ") + vector_to_string_matlab(A.get_Ax(xrv)))
      println(Const(""))
      return xrv
    }
    
    def project_onto_K(x: Exp[CVXVector]): Exp[CVXVector] = {
      var rv: Exp[CVXVector] = vector_zeros(Const(0))
      var ind: Exp[Int] = Const(0)
      //pass the constants through unmodified
      rv = vector_cat(rv, vector_select(x, ind, K.unconstrained_sz))
      ind = ind + K.unconstrained_sz
      //make the positive simplex nodes positive
      rv = vector_cat(rv, vector_positive_part(vector_select(x, ind, K.psimplex_sz)))
      ind = ind + K.psimplex_sz
      //project onto second-order cones
      for(n <- K.soc_ns) {
        val cx: Exp[CVXVector] = vector_select(x, ind, n)
        ind = ind + n
        val cz: Exp[Double] = vector_at(x, ind)
        ind = ind + Const(1)
        val norm2cx: Exp[Double] = vector_dot(cx,cx);
        if((cz*cz) >= norm2cx) {
          if(cz <= Const(0.0)) {
            //projection is onto the zero point
            rv = vector_cat(rv, vector_zeros(n + Const(1)))
          }
          else {
            //projection retains the original value
            rv = vector_cat(rv, cx)
            rv = vector_cat(rv, vector1(cz))
          }
        }
        else {
          //use the projection formula on pg447 of Boyd and Vandenberghe
          val normcx: Exp[Double] = math_sqrt(norm2cx)
          val cscale: Exp[Double] = Const(0.5)*(Const(1.0) + cz/normcx)
          rv = vector_cat(rv, vector_scale(cx, cscale))
          rv = vector_cat(rv, vector1(normcx * cscale))
        }
      }
      //project onto semidefinite cone
      for(n <- K.definite_ns) {
        //throw an error as this projection is not implemented
        throw new Exception("Definitness constraints not implemented yet.")
      }
      //return the accumulated vector
      return rv
    }
  }
}

trait ScalaGenSolverOps extends ScalaGenBase {
  val IR: SolverOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}