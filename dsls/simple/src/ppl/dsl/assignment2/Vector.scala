package ppl.dsl.assignment2

import virtualization.lms.common.{Variables, VariablesExp, BaseFatExp}
import ppl.delite.framework.ops._

/**
 * Operations
 */

//trait Vector[A] extends DeliteCollection[A]

/* type Vector[A] = Record {
  val data: Rep[DeliteArray[A]]
  val length: Rep[Int]
} */

trait VectorOps { this: SimpleVector =>

  abstract class Vector[A] extends Record with DeliteCollection[A]

  //syntax
  object Vector {
    def apply[A:Manifest](length: Rep[Int]) = vectorNew(length)
    def from[A:Manifest](length: Rep[Int])(func: Rep[Int] => Rep[A]) = vectorFromFunction(length,func)
  }

  implicit def vecToVecOps[A:Manifest](x: Rep[Vector[A]]) = new VectorOpsCls(x)

  class VectorOpsCls[A:Manifest](x: Rep[Vector[A]]) {
    def +(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vectorPlus(x,y)
    def +(y: Rep[A])(implicit n: Numeric[A], o: Overloaded1) = vectorPlusScalar(x,y)

    def *(y: Rep[A])(implicit n: Numeric[A]) = vectorTimesScalar(x,y)

    def sum(implicit n: Numeric[A]) = vectorSum(x)

    def map[B:Manifest](f: Rep[A] => Rep[B]) = vectorMap(x,f)
    def zip[B:Manifest,R:Manifest](y: Rep[Vector[B]])(f: (Rep[A], Rep[B]) => Rep[R]) = vectorZip(x,y,f)
    def reduce(f: (Rep[A], Rep[A]) => Rep[A])(implicit zero: Rep[A]) = vectorReduce(x,f,zero)

    def length = vectorLength(x)
    def isRow = vectorRow(x)
    def apply(idx: Rep[Int]) = vectorApply(x, idx)

    def pprint() = vectorPrint(x)
  }

  //operations
  def vectorNew[A:Manifest](length: Rep[Int]): Rep[Vector[A]]
  def vectorFromFunction[A:Manifest](length: Rep[Int], f: Rep[Int] => Rep[A]): Rep[Vector[A]]

  def vectorPlus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vectorPlusScalar[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]

  def vectorTimesScalar[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]

  def vectorSum[A:Manifest:Numeric](x: Rep[Vector[A]]): Rep[A]

  def vectorMap[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vectorZip[A:Manifest,B:Manifest,R:Manifest](x: Rep[Vector[A]], y: Rep[Vector[B]], f: (Rep[A], Rep[B]) => Rep[R]): Rep[Vector[R]]
  def vectorReduce[A:Manifest](x: Rep[Vector[A]], f: (Rep[A], Rep[A]) => Rep[A], zero: Rep[A]): Rep[A]

  def vectorLength[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vectorRow[A:Manifest](x: Rep[Vector[A]]): Rep[Boolean]
  def vectorApply[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int]): Rep[A]

  def vectorPrint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorOpsExp extends VectorOps with DeliteCollectionOpsExp { this: SimpleVectorExp =>

  //implemented via kernel embedding (sequential)
  case class PPrint[A:Manifest](x: Exp[Vector[A]], print: Block[Unit])
    extends DeliteOpSingleTask(print)

  //implemented via Delite ops
  abstract class VectorMap[A:Manifest,B:Manifest] extends DeliteOpMapStruct[A,B,Vector[B]] {
    val in: Exp[Vector[A]]
    def allocWithArray = data => vectorArray(data, in.row)
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class VectorMapGeneric[A:Manifest, B:Manifest](in: Exp[Vector[A]], func: Exp[A] => Exp[B]) extends VectorMap[A,B]

  abstract class VectorZip[A:Manifest,B:Manifest,R:Manifest] extends DeliteOpZipWithStruct[A,B,R,Vector[R]] {
    val inA: Exp[Vector[A]]
    val inB: Exp[Vector[B]]
    def allocWithArray = data => vectorArray(data, inA.row)
    val size = copyTransformedOrElse(_.size)(inA.length)
  }

  case class VectorZipGeneric[A:Manifest,B:Manifest,R:Manifest](inA: Exp[Vector[A]], inB: Exp[Vector[B]], func: (Exp[A], Exp[B]) => Exp[R]) extends VectorZip[A,B,R]

  case class VectorPlus[A:Manifest:Numeric](inA: Exp[Vector[A]], inB: Exp[Vector[A]]) extends VectorZip[A,A,A] {
    def func = (a,b) => a + b
  }

  case class VectorPlusFused[A:Manifest:Numeric](inA: Exp[Vector[A]], inB: Exp[Vector[A]], sA: Exp[A], sB: Exp[A]) extends VectorZip[A,A,A] {
    def func = (a,b) => (a + sA) + (b + sB)
  }

  case class VectorPlusScalar[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A]) extends VectorMap[A,A] {
    def func = e => e + s
  }

  case class VectorTimesScalar[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A]) extends VectorMap[A,A] {
    def func = e => e * s
  }

  case class VectorFromFunction[A:Manifest](length: Exp[Int], func: Exp[Int] => Exp[A]) extends DeliteOpIndexedLoopStruct[A,Vector[A]] {
    def allocWithArray = data => vectorArray(data, unit(false))
    val size = copyTransformedOrElse(_.size)(length)
  }

  abstract class VectorReduce[A:Manifest] extends DeliteOpReduceStruct[A] {
    val in: Exp[Vector[A]]
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class VectorReduceGeneric[A:Manifest](in: Exp[Vector[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A]) extends VectorReduce[A]

  case class VectorSum[A:Manifest:Numeric](in: Exp[Vector[A]]) extends VectorReduce[A] {
    def func = (a,b) => a + b
    def zero = unit(0.asInstanceOf[A])
  }

  //TODO: anonymous record?
  def vectorNew[A:Manifest](length: Exp[Int]) = struct[Vector[A]](List("Vector"), Map("data" -> DeliteArray[A](length).unsafeImmutable, "row" -> unit(false)))
  private def vectorArray[A:Manifest](data: Exp[DeliteArray[A]], row: Exp[Boolean]) = struct[Vector[A]](List("Vector"), Map("data"->data, "row"->row))
  private def infix_data[A:Manifest](x: Exp[Vector[A]]) = field[DeliteArray[A]](x, "data")
  private def infix_row[A:Manifest](x: Exp[Vector[A]]) = field[Boolean](x, "row")

  //def length[A:Manifest](x: Exp[Vector[A]]) = x.data.length
  def vectorLength[A:Manifest](x: Exp[Vector[A]]) = darray_length(x.data)
  def vectorRow[A:Manifest](x: Exp[Vector[A]]) = x.row
  def vectorApply[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int]) = darray_apply(x.data, idx)

  def vectorFromFunction[A:Manifest](length: Exp[Int], f: Exp[Int] => Exp[A]) = VectorFromFunction(length,f)

  /* def vectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x,y) match {
    case(Def(VectorPlusScalar(v1, s1)), Def(VectorPlusScalar(v2,s2))) => VectorPlusFused(v1, v2, s1, s2)
    case _ => VectorPlus(x,y)
  } */

  def vectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x,y)
  def vectorPlusScalar[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = VectorPlusScalar(x,y)
  def vectorTimesScalar[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = VectorTimesScalar(x, y)
  def vectorSum[A:Manifest:Numeric](x: Exp[Vector[A]]) = VectorSum(x)

  def vectorMap[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]): Exp[Vector[B]] = VectorMapGeneric(x,f)
  def vectorZip[A:Manifest,B:Manifest,R:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], f: (Exp[A], Exp[B]) => Exp[R]) = VectorZipGeneric(x,y,f)
  def vectorReduce[A:Manifest](x: Exp[Vector[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A]) = VectorReduceGeneric(x,f,zero)

  def vectorPrint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(PPrint(x, reifyEffectsHere(pprint_impl(x))))

  private def ifVector[A:Manifest, R](x: Exp[DeliteCollection[A]])(then: Exp[Vector[A]] => R)(orElse: => R): R = {
    if (x.Type.erasure == classOf[Vector[A]]) then(x.asInstanceOf[Exp[Vector[A]]]) else orElse
  }

  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]]): Exp[Int] = ifVector(x)(vectorLength(_))(super.dc_size(x))
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], idx: Exp[Int]): Exp[A] = ifVector(x)(vectorApply(_, idx))(super.dc_apply(x, idx))
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], idx: Exp[Int], value: Exp[A]): Exp[Unit] = ifVector(x)(v => /*reifyEffectsHere*/(darray_update(v.data, idx, value)))(super.dc_update(x,idx,value))

  /* override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] */

}

/**
 * Implementation using kernel embedding
 */
trait VectorImplOps { this: SimpleVector =>
  def pprint_impl[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: SimpleVectorCompiler with SimpleVectorLift =>

  def pprint_impl[A:Manifest](x: Rep[Vector[A]]) = {
    print("[ ")
    for (i <- 0 until x.length) {
      print(x(i)); print(" ")
    }
    print("]\\n")
  }
}
