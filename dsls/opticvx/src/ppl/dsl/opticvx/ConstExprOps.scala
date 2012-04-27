package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait ConstExprOps extends Base {

  def inputscalar(x: Rep[Double]): Rep[Expr]
}

trait ConstExprOpsExp extends ConstExprOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with OptVarOpsExp with ExprShapeOpsExp with VectorOpsExp =>

  case class ExprInputExp(u: Exp[Double]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_zeros(Const(1))
    }
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      vector_zeros(sz)
    }
    def get_b(): Exp[CVXVector] = {
      vector1(u)
    }
    
    def vexity(): Signum = Vexity.affine
    def shape(): Exp[ExprShape] = scalar()

    def vars(): Set[OptVarTr] = Set[OptVarTr]()

    override def toString(): String = "input(" + u.toString + ")"
  } 
  def inputscalar(u: Exp[Double]): Exp[Expr] = ExprInputExp(u)
}

trait ScalaGenConstExprOps extends ScalaGenBase {
  val IR: ConstExprOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}