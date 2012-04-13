package ppl.apps.assignment2

import ppl.dsl.assignment2.{SimpleVectorApplicationRunner, SimpleVectorApplication}
import reflect.RefinedManifest

object SimpleVectorAppRunner extends SimpleVectorApplicationRunner with SimpleVectorApp

trait SimpleVectorApp extends SimpleVectorApplication {

  def main() {
    testPrimitive()
    testStruct()
    testNestedPrimitive()
    testNestedStruct()
  }

  def testPrimitive() {
    println("testPrimitive:")
    val x = Vector.from(100) { e => 1 }
    val y = Vector.from(100) { e => 2 }

    val c = x * 5
    val z = x + y
    //println(z(0))
    println(z)

    val d = y.sum
    println(d)
  }

  def testNestedPrimitive() {
    println("testNestedPrimitive:")
    val x = Vector.from(10) { i => i }

    val y = x map { i =>
      Vector.from(2) { e => i }
    }
    println(y)
    //println(y(0)(0))
    //println(y.isRow) //create Array[Boolean] if present
    //println(y(0).isRow)

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

  def Complex(re: Rep[Double], im: Rep[Double]) = new Record {
    val real = re
    val imag = im
  }

  implicit def zero: Rep[Complex] = Complex(0,0)

  def testStruct() {
    println("testStruct:")
    val x = Vector.from(100) { e => Complex(1.0, -1.0) }
    //x.map(e => e.real).pprint() //TODO: eta-reduce!
    //x.map(e => e.imag).pprint()

    val z = x map { e => Complex(e.real + 1, e.imag - 1) }
    //println(z(0))
    println(z)

    val d = z reduce { (l,r) => Complex(l.real + r.real, l.imag + r.imag) }
    println(d)
  }

  def testNestedStruct() {
    println("testNestedStruct:")
    val x = Vector.from(100) { i => i }

    val y = x map { i =>
      Vector.from(10) { e => Complex(i,i) }
    }
    println(y(0)(0))

    val a = Vector.from(100) { i =>
      Vector.from(10) { j =>
        Vector.from(1) { e => Complex(i,j) } //real component can be hoisted
      }
    }
    println(a(0)(0)(0))

    val z = x map {i =>
      val r = Vector.from(10) { e => Complex(i,i) }
      r reduce { (l,r) => Complex(l.real + r.real, l.imag + r.imag) }
    }
    println(z(0))
  }

}
