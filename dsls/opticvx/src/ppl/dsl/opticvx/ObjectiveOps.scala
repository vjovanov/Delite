package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait ObjectiveOps extends Base {

  case class MinimizeObjStatement(x: Rep[Expr]) {
    def over(vs: Rep[OptVar]*) = minimize_over(x,vs)
  }
  def minimize(x: Rep[Expr]) = MinimizeObjStatement(x)

  def minimize_over(x: Rep[Expr], vs: Seq[Rep[OptVar]]): Rep[Unit]
}

trait ObjectiveOpsExp extends ObjectiveOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with ExprShapeOpsExp =>

  def minimize_over(x: Exp[Expr], vs: Seq[Exp[OptVar]]): Exp[Unit] = {
    if(!(canonicalize(x).vexity() <= Vexity.convex)) {
      throw new Exception("Could not minimize non-convex expression.")
    }
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not minimize non-scalar expression.")
    }
  }
}

trait ScalaGenObjectiveOps extends ScalaGenBase {
  val IR: ObjectiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}