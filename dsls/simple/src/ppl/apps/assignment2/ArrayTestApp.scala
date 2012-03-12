package ppl.apps.assignment2
import ppl.dsl.assignment2.{SimpleVectorApplicationRunner, SimpleVectorApplication}
import ppl.delite.framework.ops.DeliteCollection


/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object ArrayTestApp extends SimpleVectorApplicationRunner {
  def main() {
    testArrays()
  }

  abstract class MyArray[A] extends DeliteArray[A] with DeliteCollection[A] //TODO: DeliteArray implements DeliteCollection?

  def fromFunction[A:Manifest](length: Rep[Int])(func: Rep[Int] => Rep[A]): Rep[MyArray[A]] = ArrayFromFunction(length, func)
  def map[A:Manifest,B:Manifest](x: Rep[MyArray[A]])(func: Rep[A] => Rep[B]): Rep[MyArray[B]] = ArrayMap(x, func)

  case class ArrayFromFunction[A:Manifest](length: Exp[Int], func: Exp[Int] => Exp[A]) extends DeliteOpIndexedLoopStruct[A,MyArray[A]] {
    def allocWithArray = data => data.asInstanceOf[Exp[MyArray[A]]]
    val size = copyTransformedOrElse(_.size)(length)
  }

  case class ArrayMap[A:Manifest,B:Manifest](in: Exp[MyArray[A]], func: Exp[A] => Exp[B]) extends DeliteOpMapStruct[A,B,MyArray[B]] {
    def allocWithArray = data => data.asInstanceOf[Exp[MyArray[B]]]
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], idx: Exp[Int]): Exp[A] = darray_apply(x.asInstanceOf[Exp[DeliteArray[A]]], idx)

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case ArrayFromFunction(length, func) => fromFunction(f(length))(f(func))
    case ArrayMap(in, func) => map(f(in))(f(func))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  def testArrays() {
    val x = fromFunction(100){i => 1}
    val y = map(x){e => e + 1}
    println(darray_apply(y,10))
  }
}
