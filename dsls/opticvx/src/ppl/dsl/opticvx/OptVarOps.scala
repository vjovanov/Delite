package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait OptVarOps extends Base {

  def variable(sh: Rep[ExprShape]): Rep[OptVar]
  def variable(): Rep[OptVar]

  def problemsize(): Rep[Int]
}

trait OptVarOpsExp extends OptVarOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprShapeOpsExp with ExprOpsExp with VectorOpsExp =>

  trait OptVarTr extends ExprTr {
    
  }

  def canonicalize(x: Exp[OptVar]): OptVarTr = {
    x match {
      case e: OptVarTr =>
        return e
      case s: Sym[OptVar] =>
        findDefinition(s) match {
          case None =>
            throw new Exception("Couldn't canonicalize node " + x)
          case Some(TP(sym,rhs)) =>
            rhs match {
              case e: OptVarTr =>
                return e
              case _ =>
                throw new Exception("Couldn't canonicalize node " + x)
            }
        }
      case _ =>
        throw new Exception("Couldn't canonicalize node " + x)
    }
  }

  var problem_size: Exp[Int] = Const(0)

  case class OptVarExp(val offset: Exp[Int], val sh: Exp[ExprShape]) extends Def[OptVar] with OptVarTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_select(x, offset, canonicalize(sh).size)
    }
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = {
      vector_cat(vector_cat(vector_zeros(offset), y), vector_zeros(problem_size - (offset + canonicalize(sh).size)))
    }
    def get_b(): Exp[CVXVector] = {
      vector_zeros(problem_size)
    }

    def vexity(): Signum
      = Vexity.affine

    def shape(): Exp[ExprShape]
      = sh
  }

  def variable(sh: Exp[ExprShape]): Exp[OptVar] = {
    val rv = OptVarExp(problem_size,sh)
    problem_size = problem_size + canonicalize(sh).size
    return rv
  }

  def variable(): Rep[OptVar] = variable(scalar())

  def problemsize(): Exp[Int]
    = problem_size

  
}

trait ScalaGenOptVarOps extends ScalaGenBase {
  val IR: OptVarOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}