package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait AbstractMatrixOps extends Base {

}

trait AbstractMatrixOpsExp extends AbstractMatrixOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: StringOpsExp with WhileExp with MiscOpsExp with VectorOpsExp with IfThenElseExp 
    with VariablesExp with MathOpsExp =>

  trait AbstractMatrix {
    def m(): Exp[Int]
    def n(): Exp[Int]
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector]
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector]
  }

  //solves the equation Ax=b for x using the LSQR method and returns x
  def lsqr(A: AbstractMatrix, b: Exp[CVXVector], x0: Exp[CVXVector], itermax: Exp[Int]): Exp[CVXVector] = {
    //initialization code
    val beta_init = math_sqrt(vector_dot(b,b))
    val u_init = vector_scale(b,Const(1.0)/beta_init)
    val ATu_init = A.get_ATy(u_init)
    val alpha_init = math_sqrt(vector_dot(ATu_init,ATu_init))
    val v_init = vector_scale(ATu_init,Const(1.0)/alpha_init)
    val w_init = v_init
    val x_init = x0
    val phi_init = beta_init
    val rho_init = alpha_init
    //define the variables
    val beta = var_new[Double](beta_init)
    val u = var_new[CVXVector](u_init)
    val alpha = var_new[Double](alpha_init)
    val v = var_new[CVXVector](v_init)
    val w = var_new[CVXVector](w_init)
    val x = var_new[CVXVector](x_init)
    val phi = var_new[Double](phi_init)
    val rho = var_new[Double](rho_init)
    val iterct = var_new[Int](Const(0))
    //iterate
    __whileDo((iterct < itermax), {
      //bidiagonization step
      val Avau = vector_sum(A.get_Ax(readVar(v)),vector_scale(readVar(u),Const(-1.0)*readVar(alpha)))
      val beta_next = math_sqrt(vector_dot(Avau,Avau))
      val u_next = vector_scale(Avau,Const(1.0)/beta_next)
      val ATubv = vector_sum(A.get_ATy(u_next),vector_scale(readVar(v),Const(-1.0)*beta_next))
      val alpha_next = math_sqrt(vector_dot(ATubv,ATubv))
      val v_next = vector_scale(ATubv,Const(1.0)/alpha_next)
      //orthogonal transformation step
      val rho_nobar = math_sqrt(readVar(rho)*readVar(rho)+beta_next*beta_next)
      val c = rho_nobar/readVar(rho)
      val s = beta_next/readVar(rho)
      val theta = s*alpha_next
      val rho_next = Const(-1.0)*c*alpha_next
      val phi_nobar = c*readVar(phi)
      val phi_next = s*readVar(phi)
      //update x,w step
      val x_next = vector_sum(readVar(x), vector_scale(readVar(w), phi_nobar/rho_nobar))
      val w_next = vector_sum(v_next, vector_scale(readVar(w),Const(-1.0)*theta/rho_nobar))
      //set the variables
      var_assign(beta, beta_next)
      var_assign(u, u_next)
      var_assign(alpha, alpha_next)
      var_assign(v, v_next)
      var_assign(w, w_next)
      var_assign(x, x_next)
      var_assign(phi, phi_next)
      var_assign(rho, rho_next)
      var_assign(iterct, readVar(iterct) + Const(1))
    })
    //return the value converged to
    return readVar(x)
  }
}

trait ScalaGenAbstractMatrixOps extends ScalaGenBase {
  val IR: AbstractMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}