package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait ConstraintOps extends Base {
  
  def infix_>=(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def infix_<=(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def infix_==(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]

  def constrain_zero(x: Rep[Expr]): Rep[Unit]

  def constrain_nonnegative(x: Rep[Expr]): Rep[Unit]
  def constrain_secondordercone(x: Rep[Expr], z: Rep[Expr]): Rep[Unit]
  def constrain_semidefinite(x: Rep[Expr]): Rep[Unit]
}

trait ConstraintOpsExp extends ConstraintOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with ExprShapeOpsExp =>

  def infix_>=(x: Exp[Expr], y: Exp[Expr]): Exp[Unit]
    = constrain_nonnegative(x-y)

  def infix_<=(x: Exp[Expr], y: Exp[Expr]): Exp[Unit]
    = constrain_nonnegative(y-x)

  def infix_==(x: Exp[Expr], y: Exp[Expr]): Exp[Unit]
    = constrain_zero(x-y)

  def constrain_zero(x: Exp[Expr]): Exp[Unit] = {
    if(!(canonicalize(x).vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression to zero.")
    }
  }

  def constrain_nonnegative(x: Exp[Expr]): Exp[Unit] = {
    if(!(canonicalize(x).vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression to be nonnegative.")
    }
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression to be nonnegative.")
    }
  }
  def constrain_secondordercone(x: Exp[Expr], z: Exp[Expr]): Exp[Unit] = {
    if(!(canonicalize(x).vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression as X-part of second-order cone.")
    }
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeVectorExp => 
      case _ => throw new Exception("Could not constrain non-vector expression as X-part of second-order cone.")
    }
    if(!(canonicalize(z).vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression as Z-part of second-order cone.")
    }
    canonicalize(canonicalize(z).shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression as Z-part of second-order cone.")
    }
  }
  def constrain_semidefinite(x: Exp[Expr]): Exp[Unit] = {
    if(!(canonicalize(x).vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression to be semidefinite.")
    }
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeSMatrixExp => 
      case _ => throw new Exception("Could not constrain non-matrix expression to be semidefinite.")
    }
  }
}

trait ScalaGenConstraintOps extends ScalaGenBase {
  val IR: ConstraintOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}