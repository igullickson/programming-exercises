package part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import part1.Chp5._

class Chp5Spec extends AnyFlatSpec with should.Matchers {

  "toList" should "convert stream to list" in {
    Stream.empty.toList shouldBe List()
    Stream(1).toList shouldBe List(1)
    Stream(1,2,3).toList shouldBe List(1,2,3)
  }

  "toListFast" should "convert stream to list" in {
    Stream.empty.toListFast shouldBe List()
    Stream(1).toListFast shouldBe List(1)
    Stream(1,2,3).toListFast shouldBe List(1,2,3)
  }

  "take" should "return first n elements of stream" in {
    Stream.empty.take(2).toList shouldBe Stream.empty.toList
    Stream(1).take(1).toList shouldBe Stream(1).toList
    Stream(1,2,3).take(3).toList shouldBe Stream(1,2,3).toList
    Stream(1,2,3).take(2).toList shouldBe Stream(1,2).toList
    Stream(1,2).take(3).toList shouldBe Stream(1,2).toList
  }

  "takeWhile" should "return starting elements that match given predicate" in {
    val p = (i: Int) => i < 5
    Stream.empty.takeWhile(p).toList shouldBe Stream.empty.toList
    Stream(1).takeWhile(p).toList shouldBe Stream(1).toList
    Stream(1,2,3).takeWhile(p).toList shouldBe Stream(1,2,3).toList
    Stream(1,2,5,6).takeWhile(p).toList shouldBe Stream(1,2).toList
    Stream(5).takeWhile(p).toList shouldBe Stream.empty.toList
    Stream(1,2,5,6,1).takeWhile(p).toList shouldBe Stream(1,2).toList
  }

  "forAll" should "return whether all elements in stream satisfy predicate" in {
    val p = (i: Int) => i < 5
    Stream.empty.forAll(p) shouldBe true
    Stream(1).forAll(p) shouldBe true
    Stream(1,2,3).forAll(p) shouldBe true
    Stream(1,2,5,6).forAll(p) shouldBe false
    Stream(5).forAll(p) shouldBe false
    Stream(1,2,5,6,1).forAll(p) shouldBe false
  }

  "headOptionFoldRight" should "return head of stream in option if exists" in {
    Stream.empty.headOptionFoldRight shouldBe None
    Stream(1).headOptionFoldRight shouldBe Some(1)
    Stream(1,2).headOptionFoldRight shouldBe Some(1)
  }

  "map" should "apply function to each element in stream" in {
    val f = (i: Int) => i * 2
    Stream.empty.map(f).toList shouldBe Stream.empty.toList
    Stream(1).map(f).toList shouldBe Stream(2).toList
    Stream(1,2).map(f).toList shouldBe Stream(2,4).toList
    Stream(1,2,3).map(f).toList shouldBe Stream(2,4,6).toList
  }
  
  "filter" should "filter elements that don't satisfy predicate" in {
    val p = (i: Int) => i < 5
    Stream.empty.filter(p).toList shouldBe Nil
    Stream(1).filter(p).toList shouldBe List(1)
    Stream(1,2,3).filter(p).toList shouldBe List(1,2,3)
    Stream(1,2,5,6).filter(p).toList shouldBe List(1,2)
    Stream(5).filter(p).toList shouldBe Nil
    Stream(1,2,5,6,1).filter(p).toList shouldBe List(1,2,1)
  }

  "append" should "add element to end of stream" in {
    Stream.empty.append(1).toList shouldBe List(1)
    Stream(1).append(2).toList shouldBe List(1,2)
    Stream(1,2,3).append(4).toList shouldBe List(1,2,3,4)
  }

  "flatMap" should "apply function to each element in stream and flatten result" in {
    val f = (i: Int) => Stream(i - 1, i * 2)
    Stream.empty.flatMap(f).toList shouldBe Stream.empty.toList
    Stream(1).flatMap(f).toList shouldBe List(0, 2)
    Stream(1,2).flatMap(f).toList shouldBe List(0, 2, 1, 4)
    Stream(1,2,3).flatMap(f).toList shouldBe List(0, 2, 1, 4, 2, 6)
  }

}
