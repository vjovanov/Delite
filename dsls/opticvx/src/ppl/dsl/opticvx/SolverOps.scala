package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait SolverOps extends Base {

}

trait SolverOpsExp extends SolverOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with OptVarOpsExp with ExprShapeOpsExp with StringOpsExp with WhileExp with MiscOpsExp with ConstraintOpsExp with VectorOpsExp =>

  case class SolverExp(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]) extends Def[Unit]

  def matlab_print_problem(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]): Exp[Unit] = {
    println(Const("cvx_begin"))
    println(Const("variable x(") + string_valueof(sz) + Const(")"))
    for(c <- cs) {
      c match {
        case ConstrainZero(x: ExprTr) =>
          canonicalize(x.shape()) match {
            case sh: ExprShapeScalarExp =>
              val at = x.get_ATy(vector1(Const(1)), sz)
              val bt = x.get_b()
              println(Const("0 == ") + vector_to_string_matlab(at) + Const("*x + ") + vector_to_string_matlab(bt))
            case _ =>
              println(Const("nonscalar equality expression"))
          }
          

        case ConstrainNonnegative(x: ExprTr) =>
          val at = x.get_ATy(vector1(Const(1)), sz)
          val bt = x.get_b()
          println(Const("0 <= ") + vector_to_string_matlab(at) + Const("*x + ") + vector_to_string_matlab(bt))

        case ConstrainSecondOrderCone(x: ExprTr, z: ExprTr) =>
          println(Const("second-order cone constraint"))

        case ConstrainSemidefinite(x: ExprTr) =>
          println(Const("semidefinite constraint"))

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