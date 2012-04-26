package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait VectorOps extends Base {
  
}

trait VectorOpsExp extends VectorOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {

  type CVXVector = Array[Double]

  case class VectorSumExp(x: Exp[CVXVector], y: Exp[CVXVector]) extends Def[CVXVector]
  def vector_sum(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = VectorSumExp(x,y)

  case class VectorNegExp(x: Exp[CVXVector]) extends Def[CVXVector]
  def vector_neg(x: Exp[CVXVector]): Exp[CVXVector]
    = VectorNegExp(x)

  case class VectorSelectExp(x: Exp[CVXVector], offset: Exp[Int], len: Exp[Int]) extends Def[CVXVector]
  def vector_select(x: Exp[CVXVector], offset: Exp[Int], len: Exp[Int]): Exp[CVXVector]
    = VectorSelectExp(x,offset,len)

  case class VectorCatExp(x: Exp[CVXVector], y: Exp[CVXVector]) extends Def[CVXVector]
  def vector_cat(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = VectorCatExp(x,y)

  case class VectorZeros(len: Exp[Int]) extends Def[CVXVector]
  def vector_zeros(len: Exp[Int]): Exp[CVXVector]
    = VectorZeros(len)

  case class Vector1(u: Exp[Double]) extends Def[CVXVector]
  def vector1(u: Exp[Double])
    = Vector1(u)
}

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}