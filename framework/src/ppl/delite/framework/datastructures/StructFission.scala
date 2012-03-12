package ppl.delite.framework.datastructures

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatTraversal

/*trait StructFission extends FatTraversal with SimplifyTransform {
  val IR: DeliteOpsExp
  import IR._

  /*override def focusExactScopeFat[A](currentScope: List[TTP])(resultB: List[Block[Any]])(body: List[TTP] => A): A = {
    super.focusExactScopeFat(currentScope)(resultB)(body)
  }*/

  override def focusExactScopeFat[A](currentScope: List[TTP])(resultB: List[Block[Any]])(body: List[TTP] => A): A = {
    val result = resultB.map(getBlockResultFull)

    // find loops at current top level
    val Wloops = super.focusExactScopeFat(currentScope)(resultB) { levelScope =>
      levelScope collect { case e @ TTP(_, SimpleFatLoop(_,_,_)) => e } //list of all loops in current scope
    }

    val (newScope, newResultB) = if (Wloops.nonEmpty) {
      val t = new SubstTransformer

      Wloops foreach { loop => loop.rhs match {
        case SimpleFatLoop(size, v, body) => {
          if (!(loop.lhs.size == 1 && body.size == 1)) printerr("ERROR: loop fusing has occurred before struct fission")
          body(0) match {
            case c: DeliteCollectElem[_,_] => c.func match { //TODO: types
              //split collect of struct
              case Block(Def(Struct(tag,elems))) =>
                def copyLoop[B:Manifest](func: Block[B]): Exp[Array[B]] = { //TODO: Array -> DeliteArray here and in DeliteOps
                  val aV = fresh[Array[B]]
                  simpleLoop(size, v, DeliteCollectElem[B,Array[B]](aV = aV, alloc = reifyEffects(aV), cond = c.cond, func = func))
                }
                val soa = struct[Array[Any]](tag, elems.map(p=>(p._1, copyLoop(Block(p._2))(p._2.Type))))
                val outputStruct = struct[Any](List("Array","DataTable"), Map("data"->soa, "size"->soa.length)) //c.allocWithArray(soa) //need a function to instantiate the outer struct to return
                t.subst(loop.lhs(0)) = outputStruct //TODO: should not be able to map a Sym[DataTable] to a Sym[Result[Array]] (sadly they're both structs)

              case _ => //no transform
            }

            //TODO: case h: DeliteHashCollectElem[_,_,_] =>

            case _ => //no transform
          }
        }
        case _ => //no transform
        }

      }

      val (newScope, newResult) = transformAllFully(currentScope, result, t)
      val newResultB = newResult.map(Block(_))

      //TODO: changed result?
      assert(result == newResult, "TODO: patch up new result for struct fission")

      // schedule (and emit) -- clean things up
      (getFatSchedule(newScope)(newResultB), newResultB)
    }
    else (currentScope, resultB)

    super.focusExactScopeFat(newScope)(newResultB)(body)
  }

} */
