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
  self: ExprOpsExp with ExprShapeOpsExp with ConstraintOpsExp with OptVarOpsExp with SolverOpsExp with VectorOpsExp =>

  def minimize_over(x: Exp[Expr], vs: Seq[Exp[OptVar]]): Exp[Unit] = {
    val cx = canonicalize(x)
    //check that the expression to minimize is scalar and convex
    if(!(cx.vexity() <= Vexity.convex)) {
      throw new Exception("Could not minimize non-convex expression.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not minimize non-scalar expression.")
    }
    //bind all the variables to optimize over
    for(v <- vs) {
      val cv = canonicalize(v)
      if(cv.bound == true) {
        throw new Exception("Variable in optimize-set was already bound to another optimization statement.")
      }
      cv.bound = true
    }
    //accumulate the set of all variables that are connected to this objective by constraints
    var convars: Set[OptVarTr] = cx.vars()
    for(v <- vs) {
      val cv = canonicalize(v)
      convars += cv
      convars ++= cv.vars()
    }
    //iterate, gathering more vars until stable
    {
      var next_convars: Set[OptVarTr] = convars
      do {
        convars = next_convars
        for(vv <- convars) {
          for(constraint <- vv.constraints) {
            next_convars ++= constraint.vars()
          }
        }
      } while(next_convars != convars)
    }
    //verify that all the connected vars are bound
    for(v <- convars) {
      if(v.bound == false) {
        println("Found partial optimization statement over " + vs.length + " variables; not solving yet.")
        return
      }
    }
    println("Encountered optimization statement over " + vs.length + " variables (reduced to " + convars.size + "); proceeding to transform.")
    //collect all the constraints
    var constraints: Set[Constraint] = Set()
    for(v <- convars) {
      constraints ++= v.constraints
    }
    //DEBUG display the variables and constraints
    println("Partially-transformed problem: ")
    println("  variables ->")
    var strout = "    "
    for(v <- convars) {
      strout += v + " " //"(" + v.size + ") "
    }
    println(strout)
    println("  constraints ->")
    for(c <- constraints) {
      println("    " + c)
    }
    //we now assign limits to the variables
    var problem_size: Exp[Int] = Const(0)
    for(v <- convars) {
      if(v.solved == false) {
        v.lookup_offset = problem_size
        problem_size = problem_size + v.size
      }
    }
    //invoke the solver
    //val solution = fresh[CVXVector]
    //createDefinition(solution, solve(cx, constraints, problem_size))
    solve(cx, constraints, problem_size)
    //distribute the solution
    for(v <- convars) {
      if(v.solved == false) {
        v.value = null //vector_sum(v.get_Ax(solution),v.get_b())
        v.lookup_offset = null
        v.solved = true
      }
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