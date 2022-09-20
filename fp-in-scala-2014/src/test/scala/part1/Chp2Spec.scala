package part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import part1.Chp2.{fib, fibI}

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
}
