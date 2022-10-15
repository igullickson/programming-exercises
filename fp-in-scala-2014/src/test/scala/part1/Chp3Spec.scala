package part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import part1.Chp3._

class Chp3Spec extends AnyFlatSpec with should.Matchers {

  "tail" should "return the tail of a list" in {
    tail(Nil) shouldBe Nil
    tail(List(1)) shouldBe Nil
    tail(List(1,2)) shouldBe List(2)
    tail(List(1,2,3,4)) shouldBe List(2,3,4)
  }

  "setHead" should "replace the head of a list" in {
    setHead(1, Nil) shouldBe List(1)
    setHead(1, List(2,3)) shouldBe List(1,3)
    setHead(1, List(2,3,4)) shouldBe List(1,3,4)
  }

  "dropWhile" should "drop items while predicate is true" in {
    val p = (i: Int) => 6 > i

    dropWhile(Nil, p) shouldBe Nil
    dropWhile(List(1), p) shouldBe Nil
    dropWhile(List(7), p) shouldBe List(7)
    dropWhile(List(1,2,3,7,1), p) shouldBe List(7,1)
  }

  "init" should "return all but the last element in list" in {
    init(Nil) shouldBe Nil
    init(List(1)) shouldBe Nil
    init(List(1,2)) shouldBe List(1)
    init(List(1,2,3,4)) shouldBe List(1,2,3)
  }
}
