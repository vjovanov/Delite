package ppl.apps.assignment2

import ppl.dsl.assignment2.{SimpleVectorApplicationRunner, SimpleVectorApplication}
import reflect.RefinedManifest

object SimpleVectorAppRunner extends SimpleVectorApplicationRunner with SimpleVectorApp

trait SimpleVectorApp extends SimpleVectorApplication {

  def main() {
    //testPrimitive()
    //testStruct()
    //testNestedPrimitive()
    testNestedStruct()
  }

  def testPrimitive() {
    val x = Vector.from(100) { e => 1 }
    val y = Vector.from(100) { e => 2 }

    val c = x * 5
    val z = x + y
    println(z(0))

    val d = y.sum
    println(d)
  }

  def testNestedPrimitive() {
    val x = Vector.from(100) { i => i }

    val y = x map { i =>
      Vector.from(10) { e => i }
    }
    println(y(0)(0))
    println(y.isRow) //create Array[Boolean] if present
    println(y(0).isRow)

    val z = x map { i =>
      val r = Vector.from(10) {e => i }
      r.sum
    }
    println(z(0))
    println(z.isRow)
  }

  type Complex = Record {
    val real: Double
    val imag: Double
  }

  /* implicit def complexManifest = new RefinedManifest[Complex] {
    val erasure = classOf[Record]
    val fields = List(("real", manifest[Double]), ("imag", manifest[Double]))
  } */

  //abstract class Complex extends Record

  def Complex(re: Rep[Double], im: Rep[Double]) = new Record {
    val real = re
    val imag = im

  }

  implicit def zero = Complex(0,0)

  def testStruct() {
    val x = Vector.from(100) { e => Complex(1.0, -1.0) }
    //x.map(e => e.real).pprint() //TODO: eta-reduce!
    //x.map(e => e.imag).pprint()
    //x.pprint() //TODO: print(Complex) -- ops like print don't force a field read -- deal with i/o of object that doesn't really exist:
    //could override the impl to print each field, or create a runtime struct on demand and call print on that
    //soa transformation does this naturally (override of array apply), would need something similar for a generic "read" of a struct (similar to var_read?)

    //val y = Vector.from(100) { e => Complex(2.0, -2.0) }
    //println(y(0))

    //val z = x zip y { e: (Rep[Complex],Rep[Complex]) => Complex(e._1.real + e._2.real, e._1.imag + e._2.imag) }
    val z = x map { e => Complex(e.real + 1, e.imag - 1) }
    //println(z(0))

    val d = z reduce { (l,r) => Complex(l.real + r.real, l.imag + r.imag) }
    //println(d) //TODO: print(Complex)
    println(d.real)
    println(d.imag)
  }

  def testNestedStruct() {
    val x = Vector.from(100) { i => i }

    val y = x map { i => //TODO: layout of nested SoA
      Vector.from(10) { e => Complex(i,i) }
    }
    println(y(0)(0).real)
    println(y(0)(0).imag)

    val a = Vector.from(100) { i =>
      Vector.from(10) { j =>
        Vector.from(1) { e => Complex(i,j) } //real component can be hoisted
      }
    }
    println(a(0)(0)(0).real)
    println(a(0)(0)(0).imag)

    val z = x map {i =>
      val r = Vector.from(10) { e => Complex(i,i) }
      r reduce { (l,r) => Complex(l.real + r.real, l.imag + r.imag) }
    }
    println(z(0).real)
    println(z(0).imag)
  }

}
