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

  //sum of two vectors
  case class VectorSumExp(x: Exp[CVXVector], y: Exp[CVXVector]) extends Def[CVXVector]
  def vector_sum(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = VectorSumExp(x,y)

  //negation of a vector
  case class VectorNegExp(x: Exp[CVXVector]) extends Def[CVXVector]
  def vector_neg(x: Exp[CVXVector]): Exp[CVXVector]
    = VectorNegExp(x)

  //select a subrange of values from a vector
  case class VectorSelectExp(x: Exp[CVXVector], offset: Exp[Int], len: Exp[Int]) extends Def[CVXVector]
  def vector_select(x: Exp[CVXVector], offset: Exp[Int], len: Exp[Int]): Exp[CVXVector]
    = VectorSelectExp(x,offset,len)

  //concatenate two vectors
  case class VectorCatExp(x: Exp[CVXVector], y: Exp[CVXVector]) extends Def[CVXVector]
  def vector_cat(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = VectorCatExp(x,y)

  //create a zero-vector
  case class VectorZeros(len: Exp[Int]) extends Def[CVXVector]
  def vector_zeros(len: Exp[Int]): Exp[CVXVector]
    = VectorZeros(len)

  //create a size-1 vector with a particular value
  case class Vector1(u: Exp[Double]) extends Def[CVXVector]
  def vector1(u: Exp[Double])
    = Vector1(u)

  //convert a vector to matlab string representation (DEBUG)
  case class VectorToStringMatlab(x: Exp[CVXVector]) extends Def[String]
  def vector_to_string_matlab(x: Exp[CVXVector]): Exp[String]
    = VectorToStringMatlab(x)
}

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case VectorSumExp(x,y) =>
        stream.println("if(" + quote(x) + ".length != " + quote(y) + ".length)")
        stream.println("throw new Exception(\"OptiCVX Runtime Error: Vector length mismatch on sum (\" + " + quote(x) + ".length + \" vs \" + " + quote(y) + ".length + \").\")")
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length)")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = " + quote(x) + "(i) + " + quote(y) + "(i)")
        stream.println("}")

      case VectorNegExp(x) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length)")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = -" + quote(x) + "(i)")
        stream.println("}")
        
      case VectorSelectExp(x, offset, len) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(len) + ")")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = " + quote(x) + "(i + " + quote(offset) + ")")
        stream.println("}")
        
      case VectorCatExp(x, y) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length + " + quote(y) + ".length)")
        stream.println("for(i <- 0 until " + quote(x) + ".length) {")
        stream.println(quote(sym) + "(i) = " + quote(x) + "(i)")
        stream.println("}")
        stream.println("for(i <- 0 until " + quote(y) + ".length) {")
        stream.println(quote(sym) + "(i + " + quote(x) + ".length) = " + quote(y) + "(i)")
        stream.println("}")
        
      case VectorZeros(len) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(len) + ")")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = 0.0")
        stream.println("}")
        
      case Vector1(u) =>
        stream.println("val " + quote(sym) + " = new Array[Double](1)")
        stream.println(quote(sym) + "(0) = " + quote(u))
        
      case VectorToStringMatlab(x) =>
        stream.println("var stracc = \"[\"")
        stream.println("for(i <- 0 until " + quote(x) + ".length-1) {")
        stream.println("stracc = stracc + " + quote(x) + "(i).toString() + \", \"")
        stream.println("}")
        stream.println("stracc = stracc + " + quote(x) + "(" + quote(x) + ".length-1) + \"]\"")
        stream.println("val " + quote(sym) + " = stracc")
        
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}