import ppl.dsl.opticvx._

import scala.util.Random

object HelloCVXRunner extends OptiCVXApplicationRunner with HelloCVX

//symmetric cones

//variable shapes, with associated cones, can be:
//  real -> nonnegative ray
//  vector -> second-order cone
//  symmetric-matrix -> positive-semidefinite cone
//  concatenations thereof

trait HelloCVX extends OptiCVXApplication {
  /*
  def main() = {
    var u: Double
    var v: Double
    minimize (u+v) over (u,v) subjectto (
      u <= 1,
      v <= 1
    )
  }
  */

/*
cvx_begin
variables x y u
x + y <= 3
x - u >= -2
y + u <= 6
min(x,min(y,u)) >= -6
max(x,max(y,u)) <= 2
minimize max(x,u)
cvx_end
*/

  def main() = {
    //println("Cheese!")
    //val x = variable()
    //val y = variable()
    //val z = variable()
    //val a = variable()
    //val b = variable()
    //println("Vartle")
    //val z = variable(smatrix(3))
    //for(i <- 0 until 10) {
    //  println(i)
    //}
    //for(i <- 0 until 10) {
      //val a = variable()
      //introspect(i)
      //constrain_nonnegative(3)
      //a <= const_to_expr_int(3)
    //}
    //max(x,y) <= -max(x,inv(u))
    //constrain_semidefinite(z)
    //x + y <= inputscalar(3.0)
    //x - u >= inputscalar(-2.0)
    //y + u <= inputscalar(6.0)
    //min(x,min(y,u)) >= inputscalar(-6.0)
    //max(x,max(y,u)) <= inputscalar(2.0)
    //minimize (max(x,u)) over (u,x,y)

    //x >= inputscalar(-1.0)
    //y >= inputscalar(-8.0)

    //minimize (x+y) over (x,y)

    //x <= inputscalar(6.0)
    //x >= inputscalar(3.0)
    //y >= inputscalar(1.0)
    //y >= inputscalar(-2.0)
    //y <= inputscalar(2.0)
    //val J = x - y
    //minimize (J) over (x,y)

    //println("x = " + resolve(x))
    //println("y = " + resolve(y))
    //println("J = " + resolve(J))

    val c = math_sqrt(4.0)

    val x = variable()
    val y = variable()
    val z = variable()
    val J = inv(x) - y
    y >= 1
    y <= 2
    x <= 1
    c == x + y - z
    minimize (J) over (x,y,z)
    println("x = " + resolve(x))
    println("y = " + resolve(y))
    println("z = " + resolve(z))
    println("J = " + resolve(J))

    //val t = variable()
    //val x = variable(vector(2))
    //x(0) >= inputscalar(1.0)
    //x(1) >= inputscalar(1.0)
    //constrain_secondordercone(x,t)
    //minimize (t) over (x, t)
    
    //println("x = (" + resolve(x(0)) + ", " + resolve(x(1)) + ")")
    //println("t = " + resolve(t))

    //a <= inputscalar(1.0)
    //b <= inputscalar(1.0)
    //minimize (a + b) over (a,b)
    //u <= zero(scalar())
    //constrain_nonnegative(u)
    //println(resolve(u))
  }
  
  val max = cvxfun (convex) arguments (increasing, increasing) body ((x,y) => {
    val t = variable()
    t >= x
    t >= y
    minimize (t) over (t)
    t
  })

  val min = cvxfun (concave) arguments (increasing, increasing) body ((x,y) => {
    val t = variable()
    t <= x
    t <= y
    minimize (-t) over (t)
    t
  })

  val inv = cvxfun (convex) arguments (decreasing) body ((x) => {
    val v = variable(vector(2))
    val z = variable()
    v(0) == inputscalar(1.0)
    val J = z - v(1)
    x == z + v(1)
    constrain_secondordercone(v,z)
    minimize (J) over (v, z)
    J
  })

  /*
  val inv = cvxfun (convex) arguments (decreasing) body ((x) => {
    val t = variable()
    val M = variable(symmetric_matrix(2))
    M(0,0) == t
    M(1,1) == x
    M(0,1) == inputscalar(1.0)
    //M(2,1) == 1 (this will be handled by the symmetric constraint)
    constrain_semidefinite(M)
    minimize (t) over (t, M)
    t
  })
  */

  /*
  def max(x: Rep[Expr[ShapeClassScalar,Vexity#Convex]], y: Rep[Expr[ShapeClassScalar,Vexity#Convex]]): Rep[Expr[ShapeClassScalar,Vexity#Convex]] = {
    val t = variable()
    t >= x
    t >= y
    minimize (t) over (t)
    return t
  }

  def inv(x: Rep[Expr[ShapeClassScalar,Vexity#Concave]]): Rep[Expr[ShapeClassScalar,Vexity#Convex]] = {
    val t = variable()
    val M = variable(symmetric_matrix(2))
    M(1,1) == t
    M(2,2) == x
    M(1,2) == 1
    //M(2,1) == 1 (this will be handled by the symmetric constraint)
    constrain_semidefinite(M)
    minimize (t) over (t, M)
    return t
  }
  */
}

/*

We want to accumulate as our problem:
  - a set of variables (implicit in the constraint and opt sets)
  - a set of constraints, each of which is either
    - an affine constraint among the variables
    - some expression being constrained to be in a base convex cone
  - an ordered list of optimization expressions

We know that the problem is completed when each variable is associated with
exactly one optimization expression.  We then proceed to reduce the problem
to a form where we have:
  - some number of variables, each of which is either "unconstrained", or
    associated with a basic symmetric cone constraint
  - a single (matrix) affine constraint among the variables
  - a single minimization objective

The main issue here is that we have to transform from multiple objectives to
a single objective.  We do this (as in CVX) using the DCP ruleset.

Now, what properties do we need an expression to have:
  - a set of variables the expression is dependant on
  - a vexity
  - a montonicity in each variable
  - reduction rules for the expression

The standard form of an expression is the same as that for a problem, except
we choose a single output variable.

The question is: how do we modify this structure to accomodate user input
(in the form of Rep[T] values).  I don't want to have to deal with doing
Rep[Expr] objects, but rather only have Rep[Int] and Rep[Double] values.

At any point in the code, we keep track of the combined shape of all the
variables.

So, we will have each Expr have properties:
  - the optimization variable (with associated shape); not in the dependant set
  - the dependant set (of constrained variables)
  - the affine constraint (among all variables)
  - the optimization direction (minimize or maximize)

In order to put the problem into this form we will try to use linear algebra
to eliminate unconstrained variables.

I think this will actually be sufficient.  Namely:
  - the optimization direction fully determines the vexity
  - monotonicity is determined by the monotonicity equation, which uses the set of 
    constrained variables and the affine constrain to determine monotonicity
  - basically, this works like the following:
    - for each dependant variable, look at its monotonicity (from affine constraint)
    - look at the monotonicity of the optimization variable (from affine constraint)
    - compute the output monotonicity based on this formula


OK:  An expression is:
  - an affine function of the variables

*/

/*

If we have:

minimize (x0) over (x0, x1, ... , xn)
followed by
minimize (y0)

This can be reduced to:

minimize (c_x*X) over (D_x * X == 0)
minimize (c_y*X)

The minimum occurs at (X*), when:

0 <= c_y*X     for any X such that (X+X*) is in the constraint set, and
0 <= c_x*Z     for any Z such that D_x*Z = 0, and (Z+X*) is in the constraint set

*/


  /*
  def main() = {
    somefx(new PPN[Naturals.N0])(new PPN[Naturals.N0])
    somefx(new PPN[Naturals.N1])(new PPN[Naturals.N1])
    somefx(new PPN[Naturals.N2])(new PPN[Naturals.N4])


    val rnd = new Random()
    val x: Double = rnd.nextDouble()
    //x <= introspect(y)
    //introspect(min(x,y)) >= 1
    //inv(x) + y <= 4
    //minimize (x + y) over (x, y)
    //println("x = " + x + ", y = " + y)
    optimize ((x,y) =>
      minimize (x+y) subjectto (
        min(x,-max(x,y)) >= 1,
        min(x,3) >= 2,
        0 >= max(max(x,3),y)
      ))
  }
  
  class PPN[N <: Naturals.Natural]
  def somefx[N <: Naturals.Natural](z: PPN[N])(x: PPN[N#Mpy[N]])(implicit d: Naturals.Decoder[N]) {
    println("It works (output " + d.value + "!")
  }

  def infix_<=(x: Expr, y: Expr): Constraint = {
    is_nonnegative(y - x);
  }

  def infix_>=(x: Expr, y: Expr): Constraint = {
    is_nonnegative(x - y);
  }
  
  val min = introspect(cvxfun ((x,y) =>
    optimize ((t) => 
      maximize (t) subjectto (
      t <= x,
      t <= y
    ))) named "min")

  val max = introspect(cvxfun ((x,y) =>
    optimize ((t) => 
      minimize (t) subjectto (
      t >= x,
      t >= y
    ))) named "max")
  */

  /*def min(x: Expr, y: Expr): Expr = 
    optimize (t => {
      val z = x
      x = y
      y = z
      minimize (t) subjectto (
      t <= x,
      t <= y
    )})*/
  

   /*val t = OptVar(1,1)
    val v = OptVar(1,1)
    t <= v
    t <= x
    t <= y
    minimize (t) over (t) 
    return t*/
 
  /*
  def inv(x: Rep[Expr]): Rep[Expr] = {
    val t = OptVar(1,1)
    val M = OptVar(2,2)
    //M(1,1) == t
    //M(2,2) == x
    //M(1,2) == intToExpr(1)
    //M(2,1) == intToExpr(1)
    semidefinite(M)
    minimize (t) over (t, M)
    return t
  }

  def semidefinite(x: Rep[Expr]): Rep[Expr] = {
    val dummy = OptVar(1,1)
    return dummy
  }
  */
