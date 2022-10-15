package part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import part1.Chp2.{fib, fibI, fibR, isSorted, isSortedArray}

class Chp2Spec extends AnyFlatSpec with should.Matchers {

  "fib" should "return expected values" in {
    fib(1) shouldBe 0
    fib(2) shouldBe 1
    fib(3) shouldBe 1
    fib(4) shouldBe 2
    fib(10) shouldBe 34
  }

  "fibI" should "return expected values" in {
    fibI(1) shouldBe 0
    fibI(2) shouldBe 1
    fibI(3) shouldBe 1
    fibI(4) shouldBe 2
    fibI(10) shouldBe 34
  }

  "fibR" should "return expected values" in {
    fibR(1) shouldBe 0
    fibR(2) shouldBe 1
    fibR(3) shouldBe 1
    fibR(4) shouldBe 2
    fibR(10) shouldBe 34
  }

  "isSorted" should "returned expected values" in {
    def orderedInt(x: Int, y: Int): Boolean = x <= y

    isSorted(List.empty[Int], orderedInt) shouldBe true
    isSorted(List(1), orderedInt) shouldBe true
    isSorted(List(0,1,5,9,100), orderedInt) shouldBe true

    isSorted(List(2, 1), orderedInt) shouldBe false
    isSorted(List(0,1,0,5,100), orderedInt) shouldBe false
    isSorted(List(0,1,2,1,100), orderedInt) shouldBe false
    isSorted(List(0,1,2,6,5), orderedInt) shouldBe false
    isSorted(List(0,1,2,6,7,6), orderedInt) shouldBe false
  }

  "isSortedArray" should "returned expected values" in {
    def orderedInt(x: Int, y: Int): Boolean = x <= y

    isSortedArray(Array[Int](), orderedInt) shouldBe true
    isSortedArray(Array(1), orderedInt) shouldBe true
    isSortedArray(Array(0,1,5,9,100), orderedInt) shouldBe true

    isSortedArray(Array(2, 1), orderedInt) shouldBe false
    isSortedArray(Array(0,1,0,5,100), orderedInt) shouldBe false
    isSortedArray(Array(0,1,2,1,100), orderedInt) shouldBe false
    isSortedArray(Array(0,1,2,6,5), orderedInt) shouldBe false
    isSortedArray(Array(0,1,2,6,7,6), orderedInt) shouldBe false
  }
}
