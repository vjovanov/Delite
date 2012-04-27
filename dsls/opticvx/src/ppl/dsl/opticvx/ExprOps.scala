package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait ExprOps extends Base {

  def sum(x: Rep[Expr], y: Rep[Expr]): Rep[Expr]
  def neg(x: Rep[Expr]): Rep[Expr]
  def shapeof(x: Rep[Expr]): Rep[ExprShape]

  def infix_+(x: Rep[Expr], y: Rep[Expr]): Rep[Expr] = sum(x,y)
  def infix_-(x: Rep[Expr], y: Rep[Expr]): Rep[Expr] = sum(x,neg(y))

  case class ExprOpsImplicitHack(x: Rep[Expr]) {
    def unary_-(): Rep[Expr] = neg(x)
    def shape = shapeof(x)
    def apply(i: Rep[Int]) = indexat(x,i)
    def apply(i: Rep[Int], j: Rep[Int]) = indexat(x,i,j)
  }
  implicit def expropsimplicithack(x: Rep[Expr])
    = ExprOpsImplicitHack(x)

  def indexat(x: Rep[Expr], i: Rep[Int]): Rep[Expr]
  def indexat(x: Rep[Expr], i: Rep[Int], j: Rep[Int]): Rep[Expr]

  def resolve(x: Rep[Expr]): Rep[Double]

}

trait ExprOpsExp extends ExprOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with DeliteOpsExp with EffectExp {
  self: ExprShapeOpsExp with OptVarOpsExp with VectorOpsExp =>

  trait ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector]
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector]
    def get_b(): Exp[CVXVector]
    
    def vexity(): Signum
    def shape(): Exp[ExprShape]
  }

  def canonicalize(x: Exp[Expr]): ExprTr = {
    x match {
      case e: ExprTr =>
        return e
      case s: Sym[Expr] =>
        findDefinition(s) match {
          case None =>
            throw new Exception("Couldn't canonicalize node " + x)
          case Some(TP(sym,rhs)) =>
            rhs match {
              case e: ExprTr =>
                return e
              case _ =>
                throw new Exception("Couldn't canonicalize node " + x)
            }
        }
      case _ =>
        throw new Exception("Couldn't canonicalize node " + x)
    }
  }

  case class ExprSumExp(a: Exp[Expr], b: Exp[Expr]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      val ax = canonicalize(a).get_Ax(x)
      val bx = canonicalize(b).get_Ax(x)
      vector_sum(ax,bx)
    }
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = {
      val ay = canonicalize(a).get_ATy(y)
      val by = canonicalize(b).get_ATy(y)
      vector_sum(ay,by)
    }
    def get_b(): Exp[CVXVector] = {
      val ab = canonicalize(a).get_b()
      val bb = canonicalize(b).get_b()
      vector_sum(ab,bb)
    }

    def vexity(): Signum
      = canonicalize(a).vexity() + canonicalize(b).vexity()

    def shape(): Exp[ExprShape]
      = canonicalize(a).shape()
  }
  def sum(x: Exp[Expr], y: Exp[Expr]): Exp[Expr] = 
    ExprSumExp(x,y)
  

  case class ExprNegExp(a: Exp[Expr]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      val ax = canonicalize(a).get_Ax(x)
      vector_neg(ax)
    }
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = {
      val ay = canonicalize(a).get_ATy(y)
      vector_neg(ay)
    }
    def get_b(): Exp[CVXVector] = {
      val ab = canonicalize(a).get_b()
      vector_neg(ab)
    }

    def vexity(): Signum
      = -(canonicalize(a).vexity())

    def shape(): Exp[ExprShape]
      = canonicalize(a).shape()
  }
  def neg(x: Exp[Expr]): Exp[Expr] = 
    ExprNegExp(x)


  def shapeof(x: Exp[Expr]): Exp[ExprShape] =
    canonicalize(x).shape()
  
  case class ExprIndexExp(a: Exp[Expr], i: Exp[Int]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_select(canonicalize(a).get_Ax(x), i, Const(1))
    }
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = {
      canonicalize(a).get_ATy(vector_cat(vector_cat(vector_zeros(i),y),vector_zeros(problem_size-i-Const(1))))
    }
    def get_b(): Exp[CVXVector] = {
      vector_select(canonicalize(a).get_b(), i, Const(1))
    }
    
    def vexity(): Signum = {
      if(canonicalize(a).vexity() <= Vexity.affine) Vexity.affine
      else Vexity.none
    }
    def shape(): Exp[ExprShape] = scalar()
  }
  
  def indexat(x: Exp[Expr], i: Exp[Int]): Exp[Expr] = {
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeVectorExp => ExprIndexExp(x,i)
      case _ => throw new Exception("Could not index non-vector expression as a vector.")
    }
  }

  def indexat(x: Exp[Expr], i: Exp[Int], j: Exp[Int]): Exp[Expr] = {
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeSMatrixExp => ExprIndexExp(x,sh.indexof(i,j))
      case _ => throw new Exception("Could not index non-matrix expression as a matrix.")
    }
  }

  def resolve(x: Exp[Expr]): Exp[Double] = {
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not resolve non-scalar expression as double.")
    }
    Const(0.0)
  }

}

trait ScalaGenExprOps extends ScalaGenBase {
  val IR: ExprOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}