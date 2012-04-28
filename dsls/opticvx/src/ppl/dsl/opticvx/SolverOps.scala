package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
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
    with MiscOpsExp with ConstraintOpsExp with VectorOpsExp with VariablesExp =>

  case class SolverExp(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]) extends Def[Unit]

  def matlab_print_expression(x: ExprTr, sz: Exp[Int]): Exp[Unit] = {
    println(Const("AA = [];"))
    val vari = var_new[Int](Const(0))
    __whileDo(readVar(vari) < x.size, {
      val tt = vector_cat(vector_cat(vector_zeros(readVar(vari)),vector1(Const(1))),vector_zeros(x.size-readVar(vari)-Const(1)))
      val at = x.get_ATy(tt, sz)
      println(Const("AA = vertcat(AA,") + vector_to_string_matlab(at) + Const(");"))
      var_assign(vari, readVar(vari) + Const(1))
    })
    val bt = x.get_b()
    println(Const("bb = ") + vector_to_string_matlab(bt) + Const(";"))
  }
  
  def matlab_print_problem(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]): Exp[Unit] = {
    println(Const("cvx_begin"))
    println(Const("variable x(") + string_valueof(sz) + Const(")"))
    for(c <- cs) {
      println(Const("% " + c.toString()))
      c match {
        case ConstrainZero(x: ExprTr) =>
          matlab_print_expression(x,sz)
          println(Const("uu = AA*x + bb\';")) 
          println(Const("0 == uu;"))

        case ConstrainNonnegative(x: ExprTr) =>
          matlab_print_expression(x,sz)
          println(Const("uu = AA*x + bb\';")) 
          println(Const("0 <= uu;"))
          //val at = x.get_ATy(vector1(Const(1)), sz)
          //val bt = x.get_b()
          //println(Const("0 <= ") + vector_to_string_matlab(at) + Const("*x + ") + vector_to_string_matlab(bt))

        case ConstrainSecondOrderCone(x: ExprTr, z: ExprTr) =>
          matlab_print_expression(x,sz)
          println(Const("uu = AA*x + bb\';"))
          matlab_print_expression(z,sz)
          println(Const("zz = AA*x + bb\';"))
          println(Const("norm(uu) <= zz;"))

        case ConstrainSemidefinite(x: ExprTr) =>
          matlab_print_expression(x,sz)
          println(Const("uu = AA*x + bb\';"))
          val msz = canonicalize(x.shape()).asInstanceOf[ExprShapeSMatrixExp]
          println(Const("expression mm(") + string_valueof(msz.n) + ");")
          println(Const("for ii = 0:") + string_valueof(msz.n-Const(1)))
          println(Const("for jj = 0:") + string_valueof(msz.n-Const(1)))
          println(Const("kk = max(ii,jj)*(max(ii,jj)+1)/2 + min(ii,jj);"))
          println(Const("mm(ii+1,jj+1) = uu(kk+1);"))
          println(Const("end"))
          println(Const("end"))
          println("mm == semidefinite(" + msz.n + ");")

        case _ =>
          throw new Exception("Error: On solver emission, invalid constraint.")
      }
    }
    val atobj = obj.get_ATy(vector1(Const(1)), sz)
    val btobj = obj.get_b()
    println(Const("minimize ") + vector_to_string_matlab(atobj) + Const("*x + ") + vector_to_string_matlab(btobj))
    println(Const("cvx_end"))
  }
  
  //takes a set of constraints, and the problem size
  def solve(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]): Exp[Unit] = {
    matlab_print_problem(obj,cs,sz)
  }
}

trait ScalaGenSolverOps extends ScalaGenBase {
  val IR: SolverOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      /*
      case SolverExp(obj,cs,sz) =>
        println("Emitting solver code...")
        stream.println("println(\"cvx_begin\")")
        stream.println("println(\"variable x(\" + " + quote(sz) + " + \")\")")
        stream.println("println(\"cvx_end\")")
      */

      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}