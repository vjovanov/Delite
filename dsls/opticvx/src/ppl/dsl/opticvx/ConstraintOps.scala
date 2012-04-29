package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait ConstraintOps extends Base {
  
  def infix_>=(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def infix_<=(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def infix_===(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def __equal(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]

  def constrain_zero(x: Rep[Expr]): Rep[Unit]

  def constrain_nonnegative(x: Rep[Expr]): Rep[Unit]
  def constrain_secondordercone(x: Rep[Expr], z: Rep[Expr]): Rep[Unit]
  def constrain_semidefinite(x: Rep[Expr]): Rep[Unit]
}

trait ConstraintOpsExp extends ConstraintOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with ExprShapeOpsExp with OptVarOpsExp with IfThenElseExp with MathOpsExp with VectorOpsExp =>

  abstract class Constraint {
    def vars(): Set[OptVarTr]
    def valid(x: Exp[CVXVector], eps: Exp[Double]): Exp[Boolean]
    def project(x: Exp[CVXVector]): Exp[CVXVector]
    def overproject(x: Exp[CVXVector], a: Exp[Double]): Exp[CVXVector]
  }
  case class ConstrainZero(x: ExprTr) extends Constraint {
    def vars() = x.vars()
    override def toString() = "0 == " + (x).toString()

    override def valid(v: Exp[CVXVector], eps: Exp[Double]): Exp[Boolean] = {
      val ax = vector_at(vector_sum(x.get_Ax(v),x.get_b()),Const(0))
      Math.abs(ax) <= eps
    }

    override def project(v: Exp[CVXVector]): Exp[CVXVector] = {
      val sz = vector_len(v)
      val ax = vector_at(vector_sum(x.get_Ax(v),x.get_b()),Const(0))
      val at = x.get_ATy(vector1(Const(1.0)),sz)
      val a2 = vector_dot(at,at)
      vector_sum(v,vector_scale(x.get_ATy(vector1(ax),sz),Const(-1.0)*a2))
    }

    def overproject(x: Exp[CVXVector], a: Exp[Double]): Exp[CVXVector] = {
      project(x)
    }
  }
  case class ConstrainNonnegative(x: ExprTr) extends Constraint {
    def vars() = x.vars()
    override def toString() = "0 <= " + (x).toString()
    
    override def valid(v: Exp[CVXVector], eps: Exp[Double]): Exp[Boolean] = {
      val ax = vector_at(vector_sum(x.get_Ax(v),x.get_b()),Const(0))
      ax >= Const(-1.0)*eps
    }

    override def project(v: Exp[CVXVector]): Exp[CVXVector] = {
      val sz = vector_len(v)
      val ax = vector_at(vector_sum(x.get_Ax(v),x.get_b()),Const(0))
      if(ax <= Const(0.0)) {
        val at = x.get_ATy(vector1(Const(1.0)),sz)
        val a2 = vector_dot(at,at)
        vector_sum(v,vector_neg(vector_scale(x.get_ATy(vector1(ax),sz),a2)))
      }
      else {
        v
      }
    }

    def overproject(x: Exp[CVXVector], a: Exp[Double]): Exp[CVXVector] = {
      vector_sum(x,vector_scale(vector_sum(project(x),vector_neg(x)),a+Const(1.0)))
    }
  }
  case class ConstrainSecondOrderCone(x: ExprTr, z: ExprTr) extends Constraint {
    def vars() = x.vars() ++ z.vars()
    override def toString() = "norm(" + (x).toString() + ") <= " + (z).toString()
  
    override def valid(v: Exp[CVXVector], eps: Exp[Double]): Exp[Boolean] = {
      throw new Exception("Not implemented.")
    }

    override def project(v: Exp[CVXVector]): Exp[CVXVector] = {
      throw new Exception("Not implemented.")
    }

    def overproject(x: Exp[CVXVector], a: Exp[Double]): Exp[CVXVector] = {
      vector_sum(x,vector_scale(vector_sum(project(x),vector_neg(x)),a+Const(1.0)))
    }
  }
  case class ConstrainSemidefinite(x: ExprTr) extends Constraint {
    def vars() = x.vars()
    override def toString() = "semidefinite(" + (x).toString() + ")"

    override def valid(v: Exp[CVXVector], eps: Exp[Double]): Exp[Boolean] = {
      throw new Exception("Not implemented.")
    }

    override def project(v: Exp[CVXVector]): Exp[CVXVector] = {
      throw new Exception("Not implemented.")
    }

    def overproject(x: Exp[CVXVector], a: Exp[Double]): Exp[CVXVector] = {
      vector_sum(x,vector_scale(vector_sum(project(x),vector_neg(x)),a+Const(1.0)))
    }
  }

  def infix_>=(x: Exp[Expr], y: Exp[Expr]): Exp[Unit]
    = constrain_nonnegative(x-y)

  def infix_<=(x: Exp[Expr], y: Exp[Expr]): Exp[Unit]
    = constrain_nonnegative(y-x)

  def infix_===(x: Exp[Expr], y: Exp[Expr]): Exp[Unit] = {
    constrain_zero(x-y)
  }
  def __equal(x: Rep[Expr], y: Rep[Expr]): Rep[Unit] = {
    constrain_zero(x-y)
  }

  def constrain_zero(x: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    if(!(cx.vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression to zero.")
    }
    val constraint = ConstrainZero(cx)
    for(v <- cx.vars()) {
      v.constraints +:= constraint
    }
  }

  def constrain_nonnegative(x: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    if(!(cx.vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression to be nonnegative.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression to be nonnegative.")
    }
    val constraint = ConstrainNonnegative(cx)
    for(v <- cx.vars()) {
      v.constraints +:= constraint
    }
  }
  def constrain_secondordercone(x: Exp[Expr], z: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    val cz = canonicalize(z)
    if(!(cx.vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression as X-part of second-order cone.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeVectorExp => 
      case _ => throw new Exception("Could not constrain non-vector expression as X-part of second-order cone.")
    }
    if(!(cz.vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression as Z-part of second-order cone.")
    }
    canonicalize(cz.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression as Z-part of second-order cone.")
    }
    val constraint = ConstrainSecondOrderCone(cx,cz)
    for(v <- (cx.vars() ++ cz.vars())) {
      v.constraints +:= constraint
    }
  }
  def constrain_semidefinite(x: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    if(!(cx.vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression to be semidefinite.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeSMatrixExp => 
      case _ => throw new Exception("Could not constrain non-matrix expression to be semidefinite.")
    }
    val constraint = ConstrainSemidefinite(cx)
    for(v <- cx.vars()) {
      v.constraints +:= constraint
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