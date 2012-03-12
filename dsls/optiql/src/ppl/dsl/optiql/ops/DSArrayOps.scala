package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.{DataTable, Grouping}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, BaseFatExp, LoopsFatExp, IfThenElseFatExp, TupleOpsExp, ArrayOpsExp, LoopFusionOpt}
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.FieldAccessOpsExp

trait DSArrayOpsExp extends BaseFatExp with ArrayOpsExp with TupleOpsExp with LoopsFatExp with IfThenElseFatExp { this: OptiQLExp =>

  //object ToReduceElem {
  //  def unapply[T](x: Def)
  //}

  // TODO: temporarily we need to structurally recurse over group reductions. 
  // this will go away once groupBy fusion is integrated in lms-core

  object HashAccess0 {
    def unapply[T](e: Exp[T]): Option[(Exp[Int], Sym[Int], Block[_], Block[T], Exp[Int])] = e match {
      case Def(ArrayApply(
          Def(SimpleLoop(origSize, grpV, grpBody: DeliteHashCollectElem[aa,`T`,_])), idx1)) =>
        Some((origSize, grpV, grpBody.keyFunc, grpBody.valFunc, idx1))
      case _ => None
    }
  }

  object HashAccess {
    def unapply[T](e: Exp[T]): Option[(Exp[Int], Sym[Int], Block[_], Block[T], Exp[Int], Exp[Int])] = e match {
      case Def(ArrayApply(Def(ArrayApply(
          Def(SimpleLoop(origSize, grpV, grpBody: DeliteHashCollectElem[aa,`T`,_])), idx1)), idx2)) =>
        Some((origSize, grpV, grpBody.keyFunc, grpBody.valFunc, idx1, idx2))


      case Def(n@NumericPlus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2), 
                             HashAccess(origSize2, grpV2, keyFunc2, valFunc2, idx12, idx22))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_plus(valFunc.res,valFunc2.res)(n.aev,n.mev)), idx1, idx2))
      case Def(n@NumericPlus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2), y)) =>
        Some((origSize, grpV, keyFunc, Block(numeric_plus(valFunc.res,y)(n.aev,n.mev)), idx1, idx2))
      case Def(n@NumericPlus(y, HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_plus(y, valFunc.res)(n.aev,n.mev)), idx1, idx2))

      case Def(n@NumericMinus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2), 
                             HashAccess(origSize2, grpV2, keyFunc2, valFunc2, idx12, idx22))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_minus(valFunc.res,valFunc2.res)(n.aev,n.mev)), idx1, idx2))
      case Def(n@NumericMinus(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2), y)) =>
        Some((origSize, grpV, keyFunc, Block(numeric_minus(valFunc.res,y)(n.aev,n.mev)), idx1, idx2))
      case Def(n@NumericMinus(y, HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_minus(y, valFunc.res)(n.aev,n.mev)), idx1, idx2))

      case Def(n@NumericTimes(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2), 
                             HashAccess(origSize2, grpV2, keyFunc2, valFunc2, idx12, idx22))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_times(valFunc.res,valFunc2.res)(n.aev,n.mev)), idx1, idx2))
      case Def(n@NumericTimes(HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2), y)) =>
        Some((origSize, grpV, keyFunc, Block(numeric_times(valFunc.res,y)(n.aev,n.mev)), idx1, idx2))
      case Def(n@NumericTimes(y, HashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2))) =>
        Some((origSize, grpV, keyFunc, Block(numeric_times(y, valFunc.res)(n.aev,n.mev)), idx1, idx2))

      case _ => None
    }
  }
  
  
  /*object HashReduction {
    def unapply[T](e: Exp[T]) = e match {
      case Def(SimpleLoop(nestedSize /*Def(ArrayLength(input))*/, redV, redBody: DeliteReduceElem[cc])) =>
        Console.println("******* investigating nested collect reduce " + redBody)
        Console.println("size " + nestedSize + "=" + (nestedSize match {case Def(x) => x}))
        redBody.func match {
          case Block(HashAccess(origSize, grpV, keyFunc:Block[aa], valFunc, idx1, idx2)) =>
            Console.println("******* TADAAA!!! ")
            Console.println(""+idx1 + " " + v)
            Console.println(""+idx2 + " " + redV)
            assert(idx1 == v, "TODO: case not handled" + idx1 + " " + grpV)
            assert(idx2 == redV, "TODO: case not handled")
            Some(origSize, grpV, keyFun, valFunc, redBody.zero, redBody.rV, redBody.rFunc)
          case _ => None
        }
      case Def(ArrayLength(DefHashAccess(origSize, grpV, keyFunc, valFunc, idx1, idx2))) =>
        Some(origSize, grpV, keyFun)
      case _ => None
    }
  }*/


  // overrides for optimization
  override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case b: DeliteCollectElem[A,Array[A]] => b.func match { // unchecked!
      // split collect of struct
      case Block(Def(Struct(tag, elems))) => 
        assert(b.alloc == Block(b.aV), "TODO: only works on simple arrays for now")
        def copyLoop[B:Manifest](func: Block[B]): Exp[Array[B]] = {
          val aV = fresh[Array[B]]
          simpleLoop(size, v, DeliteCollectElem[B,Array[B]](aV = aV, alloc = reifyEffects(aV), cond = b.cond, func = func))
        }
        
        struct[A]("Array"::tag, elems.map(p=>(p._1, copyLoop(Block(p._2))(p._2.Type))))

      // collect(reduce(hashCollect)) --> hashReduce
      case Block(Def(SimpleLoop(nestedSize /*Def(ArrayLength(input))*/, redV, redBody: DeliteReduceElem[cc]))) =>
        // sz is not guaranteed to match ....
        Console.println("******* investigating nested collect reduce " + redBody)
        Console.println("size " + nestedSize + "=" + (nestedSize match {case Def(x) => x}))
        //recurseGroupReduce(redBody.func.res, v, redV, redBody)
        redBody.func match {
          case Block(HashAccess(origSize, grpV, keyFunc:Block[aa], valFunc, idx1, idx2)) =>
            Console.println("******* TADAAA!!! ")
            Console.println(""+idx1 + " " + v)
            Console.println(""+idx2 + " " + redV)
            assert(idx1 == v, "TODO: case not handled" + idx1 + " " + grpV)
            assert(idx2 == redV, "TODO: case not handled")
            
            simpleLoop(origSize, grpV, DeliteHashReduceElem(
              keyFunc = keyFunc, valFunc = valFunc, 
              zero = redBody.zero, rV = redBody.rV, rFunc = redBody.rFunc))
          case _ =>
            super.simpleLoop(size, v, body)
        }
      // hash collect with average -- TODO: generalize??
      case Block(Def(ArrayLength(HashAccess0(origSize, grpV, keyFunc, valFunc, idx1)))) =>
        Console.println("******* TADAAA!!! (count)")
        assert(idx1 == v, "TODO: case not handled" + idx1 + " " + v)
        val rV = (fresh[Int],fresh[Int])
        simpleLoop(origSize, grpV, DeliteHashReduceElem(
          keyFunc = keyFunc, valFunc = Block(Const(1)), 
          zero = Block(Const(0)), rV = rV, rFunc = reifyEffects(numeric_plus(rV._1,rV._2))))
        
      // hash collect with average -- TODO: generalize??
      case Block(Def(d@NumericDivide(Def(SimpleLoop(nestedSize1 /*Def(ArrayLength(input))*/, redV1, redBody1: DeliteReduceElem[cc1])), 
                                   Def(SimpleLoop(nestedSize2 /*Def(ArrayLength(input))*/, redV2, redBody2: DeliteReduceElem[cc2])) ))) =>
        // sz is not guaranteed to match ....
        Console.println("******* investigating nested collect reduce / reduce " + redBody1 + "," + redBody2)

        // FIXME: code duplication is bad. extract common functionality.

        (redBody1.func, redBody2.func) match {
          case (Block(HashAccess(origSize, grpV, keyFunc, valFunc1, idxA1, idxB1)),
            valFunc2 @ Block(Const(_))) =>

           Console.println("******* TADAAA!!! (average)")
           assert(idxA1 == v, "TODO: case not handled")
           assert(idxB1 == redV1, "TODO: case not handled")

           val l1 = simpleLoop[A](origSize, grpV, DeliteHashReduceElem(
             keyFunc = keyFunc, valFunc = valFunc1, 
             zero = redBody1.zero, rV = redBody1.rV, rFunc = redBody1.rFunc))

           val l2 = simpleLoop[A](origSize, grpV, DeliteHashReduceElem(
             keyFunc = keyFunc, valFunc = valFunc2, 
             zero = redBody2.zero, rV = redBody2.rV, rFunc = redBody2.rFunc))

           arraySelect[cc1](size) { i => 
             numeric_divide(
               array_apply(l1.asInstanceOf[Exp[Array[cc1]]],i)(mtype(d.mev)),
               array_apply(l2.asInstanceOf[Exp[Array[cc1]]],i)(mtype(d.mev))
              )(d.aev.asInstanceOf[Numeric[cc1]],mtype(d.mev))
            } (mtype(d.mev)) .asInstanceOf[Exp[A]]

          case _ =>
            super.simpleLoop(size, v, body)
        }
        
      // remove identity collect FIXME: length check!!
      case Block(Def(ArrayApply(xs,`v`))) if b.cond == Nil && !size.isInstanceOf[Const[_]] /*&& array_length(xs) == size*/ => 
        xs.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
      case _ => super.simpleLoop(size, v, body)
    }

    case b: DeliteHashCollectElem[k,A,Array[Array[A]]] => b.valFunc match { // unchecked!
      // split hashcollect of struct values
      case Block(Def(Struct(tag, elems))) => 
        //assert(b.alloc == Block(b.aV), "TODO: only works on simple arrays for now")
        def copyLoop[B:Manifest](valFunc: Block[B]): Exp[Array[Array[B]]] = {
          //val aV = fresh[Array[B]]
          simpleLoop(size, v, DeliteHashCollectElem[k,B,Array[Array[B]]](cond = b.cond, keyFunc = b.keyFunc, valFunc = valFunc))
        }

        struct[A]("Array"::"Array"::tag, elems.map(p=>(p._1, copyLoop(Block(p._2))(p._2.Type))))

      //TODO!!case Block(Def(ArrayApply(xs,v))) if b.cond == Nil && array_length(xs) == size => xs.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
      case _ => super.simpleLoop(size, v, body)
    }
    case _ => super.simpleLoop(size, v, body)
  }

  
  // write these as: (not so easy, polymorphic function)
  // def array_apply = distributeStruct(a) ( e => array_apply(e,i) ) getOrElse (super.array_apply(a,i))

  override def array_apply[A:Manifest](a: Rep[Array[A]], i: Rep[Int]): Rep[A] = a match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[A]]])) =>
      assert(pre == "Array")
      def unwrap[A](e:Exp[Array[A]],m:Manifest[Array[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ => 
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: array_apply expect type Array[A] but got "+m+" for input " + e.toString + "/" + Def.unapply(e).toString); mtype(manifest[Any]) }
      }
      struct[A](tag, elems.map(p=>(p._1,array_apply(p._2, i)(unwrap(p._2,p._2.Type)))))
    case _ => super.array_apply(a,i)
  }
  

  // NOTE: We should think about structs with properties/data common to all the fields.
  // 1) A soa-transformed array is a Struct of arrays that all have the same length. So what
  // is 'the length' of the transformed array? Currently we forward to field #0, which works
  // but is not ideal (holds on to that field even if not used otherwise, complicates 
  // matching on length node).
  // 2) An Option[Struct] value, represented as a Struct, is set or empty for all the fields
  // together, not individually. What is the result of .isEmpty on the transformed value?

  override def array_length[A:Manifest](a: Rep[Array[A]]): Rep[Int] = a match {
    case Def(SimpleLoop(size,v,body)) => body match {
      case b: DeliteCollectElem[_,_] if b.cond == Nil => size
      case _ => super.array_length(a) // TODO: might want to construct reduce elem with same condition?
    }
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[A]]])) =>
      assert(pre == "Array")
      val ll = elems.map(p=>array_length(p._2)) // all arrays must have same length! TODO: might not want to gen code for this
      //ll reduceLeft { (a1,a2) => assert(a1 == a2, "lengths don't match for " + a + "," + elems + ": "+ll); a1 }
      ll.head
    case _ => super.array_length(a)
  }
  

  // ******** api *********

  def arraySelect[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A]): Exp[Array[A]] = {
    val v = fresh[Int]
    val aV = fresh[Array[A]]
    simpleLoop(size,v,DeliteCollectElem[A, Array[A]](
      aV = aV,
      alloc = reifyEffects(aV),
      func = reifyEffects(func(v))
    ))
  }
  
  def arrayWhere[A:Manifest](size: Exp[Int])(cond: Exp[Int]=>Exp[Boolean])(func: Exp[Int]=>Exp[A]): Exp[Array[A]] = {
    val v = fresh[Int]
    val aV = fresh[Array[A]]
    simpleLoop(size,v,DeliteCollectElem[A, Array[A]](
      aV = aV,
      alloc = reifyEffects(aV),
      func = reifyEffects(func(v)),
      cond = reifyEffects(cond(v))::Nil
    ))
  }

  case class ArrayFlatten[A](data: Exp[Array[Array[A]]]) extends Def[Array[A]]
  def arrayFlatten[A:Manifest](data: Exp[Array[Array[A]]]): Exp[Array[A]] = data match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[Array[A]]]])) =>
      assert(pre == "Array")
      def unwrap[A](m:Manifest[Array[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ => 
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: arrayFlatten expect type Array[A] but got "+m+" for input " + data.toString + "/" + elems.toString); mtype(manifest[Any]) }
      }
      struct[Array[A]](tag, elems.map(p=>(p._1, arrayFlatten(p._2)(unwrap(unwrap(p._2.Type))))))
    case _ => ArrayFlatten(data)
  }
    
  // ---- hashing

  def arrayDistinct[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A]): Exp[Array[A]] = {
    val v = fresh[Int]
    val rV = (fresh[A], fresh[A])
    simpleLoop(size,v,DeliteHashReduceElem[A,A, Array[A]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(func(v)),
      valFunc = reifyEffects(func(v)),
      zero = reifyEffects(unit(null).asInstanceOfL[A]),
      rV = rV,
      rFunc = reifyEffects(rV._2)
    ))
  }
  
  def arrayGroup[K:Manifest,V:Manifest](size: Exp[Int])(keyFunc: Exp[Int]=>Exp[K])(valFunc: Exp[Int]=>Exp[V]): Exp[Array[Array[V]]] = {
    val v = fresh[Int]
    //val aV = fresh[Array[A]]
    simpleLoop(size,v,DeliteHashCollectElem[K,V, Array[Array[V]]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(keyFunc(v)),
      valFunc = reifyEffects(valFunc(v))
      //cond = reifyEffects(cond(v))::Nil
    ))
  }

  def indexBuild[K:Manifest](size: Exp[Int])(keyFunc: Exp[Int]=>Exp[K]): Exp[Map[K,Int]] = {
    val v = fresh[Int]
    //val aV = fresh[Array[A]]
    simpleLoop(size,v,DeliteHashIndexElem[K,Map[K,Int]](
      //aV = aV,
      //alloc = reifyEffects(aV),
      keyFunc = reifyEffects(keyFunc(v))
      //valFunc = reifyEffects(valFunc(v))
      //cond = reifyEffects(cond(v))::Nil
    ))
  }

  case class IndexLookup[K](map: Exp[Map[K,Int]], key: Exp[K]) extends Def[Int]
  
  def indexLookup[K:Manifest](map: Exp[Map[K,Int]], key: Exp[K]): Exp[Int] = {
    IndexLookup(map, key)
  }



  // ---- reduce

  def reducePlain[A:Manifest](size: Exp[Int])(func: Exp[Int]=>Exp[A])(zero: =>Exp[A])(red: (Exp[A],Exp[A])=>Exp[A]): Exp[A] = {
    val v = fresh[Int]
    val rV = (fresh[A], fresh[A])
    simpleLoop(size,v,DeliteReduceElem[A](
      func = reifyEffects(func(v)),
      zero = reifyEffects(zero),
      rV = rV,
      rFunc = reifyEffects(red(rV._1, rV._2)),
      stripFirst = false
    ))
  }

  def reduceTuple[A:Manifest,B:Manifest](size: Exp[Int])(funcA: Exp[Int]=>Exp[A], funcB: Exp[Int]=>Exp[B])(zeroA: =>Exp[A], zeroB: =>Exp[B])(red: ((Exp[A],Exp[B]),(Exp[A],Exp[B]))=>(Block[A],Block[B])): Exp[A] = {
    /*lazy val body: Def[R] = copyBodyOrElse(DeliteReduceTupleElem[R,Q](
      func = /*reifyEffects*/(zip(dc_apply(inA,v), dc_apply(inB,v))), //FIXME: tupled reify
      zero = this.zero,
      rVPar = this.rV,
      rVSeq = this.rV,
      rFuncPar = /*reifyEffects*/(reduce(rV._1, rV._2)),  //FIXME: tupled reify
      rFuncSeq = /*reifyEffects*/(reduce(rV._1, rV._2)),  //FIXME: tupled reify
      stripFirst = (!isPrimitiveType(manifest[R]) || !isPrimitiveType(manifest[R])) && !this.mutable
    ))*/
    val v = fresh[Int]
    val rVPar = ((fresh[A], fresh[B]), (fresh[A], fresh[B]))
    val rVSeq = ((fresh[A], fresh[B]), (fresh[A], fresh[B]))
    simpleLoop(size,v,DeliteReduceTupleElem[A,B](
      func = (reifyEffects(funcA(v)), reifyEffects(funcB(v))),
      zero = (reifyEffects(zeroA), reifyEffects(zeroB)),
      rVPar = rVPar,
      rVSeq = rVSeq,
      rFuncPar = /*reifyEffects*/(red(rVPar._1, rVPar._2)),  //FIXME: tupled reify
      rFuncSeq = /*reifyEffects*/(red(rVSeq._1, rVSeq._2)),  //FIXME: tupled reify
      cond = Nil,
      stripFirst = false
    ))
  }


  // ---- sorting

  case class ArraySort(len: Exp[Int], v: (Sym[Int],Sym[Int]), comp: Block[Boolean]) extends Def[Array[Int]]

  def arraySort(size: Exp[Int])(compFunc: (Exp[Int],Exp[Int])=>Exp[Boolean]): Exp[Array[Int]] = {
    val v = (fresh[Int],fresh[Int])
    ArraySort(size, v, reifyEffects(compFunc(v._1,v._2)))
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArraySort(len, v, comp) => syms(v) ++ effectSyms(comp)
    case _ => super.boundSyms(e)
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case ArraySort(len, v, comp) => toAtom(ArraySort(f(len),(f(v._1).asInstanceOf[Sym[Int]],f(v._2).asInstanceOf[Sym[Int]]),f(comp)))
    case IndexLookup(map, key) => toAtom(IndexLookup(f(map),f(key)))
    case ArrayFlatten(data) => toAtom(ArrayFlatten(f(data)))(mtype(manifest[A]))
    //case Field(o,key,manif) => toAtom(Field(f(o),key,manif))(mtype(manifest[A])) // TODO: shouldn't be here
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]



  // override -- shouldn't belong here
  //override def struct[T:Manifest](tag: List[String], elems: Map[String, Rep[Any]]): Rep[T] =
  //  toAtom(GenericStruct[T](tag, elems))(mtype(manifest[Map[String,Any]]))


  //case class CharTuple(a: Exp[Char], b: Exp[Char]) extends Def[(Char,Char)]
  /*override def make_tuple2[A:Manifest,B:Manifest](t: (Exp[A],Exp[B])): Exp[(A,B)] = 
    if (manifest[A] == manifest[Char] && manifest[B] == manifest[Char])
      toAtom(ETuple2(t._1, t._2))(mtype(manifest[Int]))
    else super.make_tuple2(t)*/
}


trait ScalaGenDSArrayOps extends ScalaGenFat with LoopFusionOpt {
  val IR: DSArrayOpsExp with OptiQLExp
  import IR._  

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayApply(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a @ Def(SimpleLoop(_,_,_:DeliteCollectElem[_,_]))) => Some(a) // exclude hash collect (?)
    case _ => super.unapplySimpleDomain(e)
  }
  

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case e@ETuple2(a, b) if e.m1 == manifest[Char] && e.m2 == manifest[Char] =>
      emitValDef(sym, "("+ quote(a) + ".toInt << 16) + " + quote(b)) 
    case a@Tuple2Access1(t) if a.m == manifest[Char] =>
      emitValDef(sym, "((" + quote(t) + " & 0xffff0000) >>> 16).toChar")
    case a@Tuple2Access2(t) if a.m == manifest[Char] =>
      emitValDef(sym, "(" + quote(t) + " & 0xffff).toChar")
    case IndexLookup(map, key) =>
      emitValDef(sym, quote(map) + ".get(" + quote(key) + ")") // it's a HashMapImpl object, so get instead of apply
    case ArrayFlatten(data) =>
      emitValDef(sym, quote(data) + ".flatten")
    case ArraySort(len, (v1,v2), comp) =>
      emitValDef(sym, "{"/*}*/)
      stream.println("val array = new Array[Integer](" + quote(len) + ")")
      stream.println("var i=0; while (i < array.length) {array(i)=i;i+=1}")
      stream.println("//FIXME: probably inefficient because of boxing")
      stream.println("val comp: java.util.Comparator[Integer] = new java.util.Comparator[Integer] { def compare("+quote(v1)+": Integer,"+quote(v2)+": Integer) = {"/*}}*/)
      emitBlock(comp)
      stream.println("if ("+quote(getBlockResult(comp))+") -1 else 1")
      stream.println(/*{{*/"}}")
      stream.println("java.util.Arrays.sort(array, comp)")
      stream.println("array map { e => e.toInt }")
      stream.println(/*{*/"}")
    case _ => super.emitNode(sym, rhs)
  }
  
  
}
