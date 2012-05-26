import ppl.dsl.opticvx._

import scala.util.Random

object HelloCVXRunner extends OptiCVXApplicationRunner with HelloCVX

trait HelloCVX extends OptiCVXApplication with OptiCVXLibrary {
  def main() = {
    val v = variable(vector(2))
    v(0) == 3.0
    v(1) == 4.0
    val J = norm2(v)
    minimize (J) over (v)
    println("norm2([3,4]) = " + resolve(J))
  }
}
