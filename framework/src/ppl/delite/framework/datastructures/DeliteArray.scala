package ppl.delite.framework.datastructures

import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.common._
import reflect.RefinedManifest

trait DeliteArrayOps extends Base {

  trait DeliteArray[T] // TBD: extends DeliteCollection or not?

  object DeliteArray {
    def apply[T:Manifest](length: Rep[Int]) = darray_new(length)
  }

  implicit def repDArrayToDArrayOps[T:Manifest](da: Rep[DeliteArray[T]]) = new DeliteArrayOpsCls(da)

  class DeliteArrayOpsCls[T:Manifest](da: Rep[DeliteArray[T]]) {
    def length: Rep[Int] = darray_length(da)
    def apply(i: Rep[Int]): Rep[T] = darray_apply(da,i)
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = darray_update(da,i,x)
  }

  def darray_new[T:Manifest](length: Rep[Int]): Rep[DeliteArray[T]]
  def darray_length[T:Manifest](da: Rep[DeliteArray[T]]): Rep[Int]
  def darray_apply[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int]): Rep[T]
  def darray_update[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]

}

trait DeliteArrayOpsExp extends DeliteArrayOps with StructExp with EffectExp {
  this: DeliteOpsExp =>

  case class DeliteArrayNew[T:Manifest](length: Exp[Int]) extends Def[DeliteArray[T]]
  case class DeliteArrayLength[T:Manifest](da: Exp[DeliteArray[T]]) extends Def[Int]
  case class DeliteArrayApply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) extends Def[T]
  case class DeliteArrayUpdate[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) extends Def[Unit]

  def darray_new[T:Manifest](length: Exp[Int]) = reflectMutable(DeliteArrayNew[T](length))
  def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = reflectPure(DeliteArrayLength[T](da))
  def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = reflectPure(DeliteArrayApply[T](da,i))
  def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) = reflectWrite(da)(DeliteArrayUpdate[T](da,i,x))

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case DeliteArrayLength(da) => darray_length(f(da))
    case DeliteArrayApply(da,i) => darray_apply(f(da),f(i))
    case Reflect(DeliteArrayNew(l), u, es) => reflectMirrored(Reflect(DeliteArrayNew(f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DeliteArrayApply(l,r), u, es) => reflectMirrored(Reflect(DeliteArrayApply(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DeliteArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(DeliteArrayUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

/*trait DeliteArrayOpsExpOpt extends DeliteArrayOpsExp with StructExpOptCommon {
  this: DeliteOpsExp =>

  //forwarders to appease the manifest craziness in the pattern-matching below
  private def dnew[T:Manifest](length: Exp[Int]): Rep[DeliteArray[T]] = darray_new(length)
  private def dlength[T:Manifest](da: Exp[DeliteArray[T]]): Rep[Int] = darray_length(da)
  private def dapply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]): Rep[T] = darray_apply(da,i)
  private def dupdate[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]): Rep[Unit] = darray_update(da,i,x)

  private def darrayManifest(arg: Manifest[Any]) = new Manifest[DeliteArray[Any]] {
    val erasure = classOf[DeliteArray[Any]]
    override val typeArguments = List(arg)
  }

  override def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      dlength(elems.head._2)(elems.head._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]])
    case _ => structType match {
      case Some(elems) => dlength(field[DeliteArray[Any]](da.asInstanceOf[Exp[Struct]],elems.head._1)(darrayManifest(elems.head._2)))(elems.head._2)
      case None => super.darray_length(da)
    }
  }

  override def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      struct[T](elems.map(p=>(p._1, dapply(p._2,i)(p._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]]))))
    case _ => structType match {
      case Some(elems) => struct[T](elems.map(p=>(p._1, dapply(field[DeliteArray[Any]](da.asInstanceOf[Exp[Struct]],p._1)(darrayManifest(p._2)),i)(p._2))))
      case None => super.darray_apply(da,i)
    }
  }

  override def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      elems.foreach(p=>dupdate(p._2,i,field[T](x.asInstanceOf[Exp[Struct]],p._1))(p._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]]))
    case _ => structType match {
      case Some(elems) => elems.foreach(p=>dupdate(field[DeliteArray[Any]](da.asInstanceOf[Exp[Struct]],p._1)(darrayManifest(p._2)),i,field[T](x.asInstanceOf[Exp[Struct]],p._1))(p._2))
      case None => super.darray_update(da,i,x)
    }
  }

  override def darray_new[T:Manifest](length: Exp[Int]) = structType match {
    case Some(elems) => struct[DeliteArray[T]](elems.map(p=>(p._1, dnew(length)(p._2))))
    case None => super.darray_new(length)
  }

}*/

trait DeliteArrayOpsExpOpt extends DeliteArrayOpsExp with StructExpOptCommon {
  this: DeliteOpsExp =>

  //TODO: choosing the length of the first array creates an unnecessary dependency (all arrays must have same length)
  override def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = da match {
    case Def(Loop(size,_,b:DeliteCollectElem[_,_])) if b.cond == Nil => size
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      darray_length(elems.head._2)(elems.head._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]])
    case _ => super.darray_length(da)
  }

  override def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      struct[T](elems.map(p=>(p._1, darray_apply(p._2,i)(p._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]]))))
    case _ => super.darray_apply(da, i)
  }

  override def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      elems.foreach(p=>darray_update(p._2,i,field[T](x.asInstanceOf[Exp[Record]],p._1))(p._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]]))
    case _ => super.darray_update(da, i, x)
  }

  //forwarder to appease type-checker
  /* private def dnew[T:Manifest](length: Exp[Int]): Rep[DeliteArray[T]] = darray_new(length)

  override def darray_new[T:Manifest](length: Exp[Int]) = manifest[T] match {
    case rm: RefinedManifest[T] => struct[DeliteArray[T]](rm.fields.map(p=>(p._1,dnew(length)(p._2))):_*)
    case _ => super.darray_new(length)
  } */

}

trait DeliteArrayFatExp extends DeliteArrayOpsExpOpt with StructFatExpOptCommon {
  this: DeliteOpsExp =>
}

trait ScalaGenDeliteArrayOps extends ScalaGenFat {
  val IR: DeliteArrayFatExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DeliteArrayNew(length) =>
      emitValDef(sym, "new Array[" + remap(sym.Type.typeArguments(0)) + "](" + quote(length) + ")")
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + ".length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ") = " + quote(x))
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => m.typeArguments(0) match {
      case s if s <:< manifest[Record] =>
        println("WARNING: emitting non-transformed array-of-struct")
        structName(m)
      case arg => "Array[" + remap(arg) + "]"
    }
    case _ => super.remap(m)
  }

  override def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = e match {
    case DeliteArrayApply(da, idx) => Some((da,idx))
    case _ => super.unapplySimpleIndex(e)
  }

  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case DeliteArrayLength(da) => Some(da)
    case _ => super.unapplySimpleDomain(e)
  }

}
