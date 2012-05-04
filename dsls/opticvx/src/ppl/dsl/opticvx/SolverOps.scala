package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, Base}
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
    with VariablesExp with MathOpsExp with AbstractMatrixOpsExp =>
  
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
    new Problem(A,b,c,K).solve()
  }
  
  class Problem(A: AbstractMatrix, b: Exp[CVXVector], c: Exp[CVXVector], K: SymmetricCone) {
    def solve(): Exp[CVXVector] = {
      vector_zeros(A.n())
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