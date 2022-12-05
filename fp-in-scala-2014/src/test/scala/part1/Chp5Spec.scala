package part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import part1.Chp5._

class Chp5Spec extends AnyFlatSpec with should.Matchers {

  "toList" should "convert stream to list" in {
    Stream.empty.toList shouldBe List()
    Stream(1).toList shouldBe List(1)
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  "toListFast" should "convert stream to list" in {
    Stream.empty.toListFast shouldBe List()
    Stream(1).toListFast shouldBe List(1)
    Stream(1, 2, 3).toListFast shouldBe List(1, 2, 3)
  }

  "take" should "return first n elements of stream" in {
    Stream.empty.take(2).toList shouldBe Stream.empty.toList
    Stream(1).take(1).toList shouldBe Stream(1).toList
    Stream(1, 2, 3).take(3).toList shouldBe Stream(1, 2, 3).toList
    Stream(1, 2, 3).take(2).toList shouldBe Stream(1, 2).toList
    Stream(1, 2).take(3).toList shouldBe Stream(1, 2).toList
  }

  "takeWhile" should "return starting elements that match given predicate" in {
    val p = (i: Int) => i < 5
    Stream.empty.takeWhile(p).toList shouldBe Stream.empty.toList
    Stream(1).takeWhile(p).toList shouldBe Stream(1).toList
    Stream(1, 2, 3).takeWhile(p).toList shouldBe Stream(1, 2, 3).toList
    Stream(1, 2, 5, 6).takeWhile(p).toList shouldBe Stream(1, 2).toList
    Stream(5).takeWhile(p).toList shouldBe Stream.empty.toList
    Stream(1, 2, 5, 6, 1).takeWhile(p).toList shouldBe Stream(1, 2).toList
  }

  "forAll" should "return whether all elements in stream satisfy predicate" in {
    val p = (i: Int) => i < 5
    Stream.empty.forAll(p) shouldBe true
    Stream(1).forAll(p) shouldBe true
    Stream(1, 2, 3).forAll(p) shouldBe true
    Stream(1, 2, 5, 6).forAll(p) shouldBe false
    Stream(5).forAll(p) shouldBe false
    Stream(1, 2, 5, 6, 1).forAll(p) shouldBe false
  }

  "headOptionFoldRight" should "return head of stream in option if exists" in {
    Stream.empty.headOptionFoldRight shouldBe None
    Stream(1).headOptionFoldRight shouldBe Some(1)
    Stream(1, 2).headOptionFoldRight shouldBe Some(1)
  }

  "map" should "apply function to each element in stream" in {
    val f = (i: Int) => i * 2
    Stream.empty.map(f).toList shouldBe Stream.empty.toList
    Stream(1).map(f).toList shouldBe Stream(2).toList
    Stream(1, 2).map(f).toList shouldBe Stream(2, 4).toList
    Stream(1, 2, 3).map(f).toList shouldBe Stream(2, 4, 6).toList
  }

  "filter" should "filter elements that don't satisfy predicate" in {
    val p = (i: Int) => i < 5
    Stream.empty.filter(p).toList shouldBe Nil
    Stream(1).filter(p).toList shouldBe List(1)
    Stream(1, 2, 3).filter(p).toList shouldBe List(1, 2, 3)
    Stream(1, 2, 5, 6).filter(p).toList shouldBe List(1, 2)
    Stream(5).filter(p).toList shouldBe Nil
    Stream(1, 2, 5, 6, 1).filter(p).toList shouldBe List(1, 2, 1)
  }

  "append" should "add element to end of stream" in {
    Stream.empty.append(1).toList shouldBe List(1)
    Stream(1).append(2).toList shouldBe List(1, 2)
    Stream(1, 2, 3).append(4).toList shouldBe List(1, 2, 3, 4)
  }

  "flatMap" should "apply function to each element in stream and flatten result" in {
    val f = (i: Int) => Stream(i - 1, i * 2)
    Stream.empty.flatMap(f).toList shouldBe Stream.empty.toList
    Stream(1).flatMap(f).toList shouldBe List(0, 2)
    Stream(1, 2).flatMap(f).toList shouldBe List(0, 2, 1, 4)
    Stream(1, 2, 3).flatMap(f).toList shouldBe List(0, 2, 1, 4, 2, 6)
  }

  "constant" should "generate infinite Stream of a given value" in {
    constant(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)
    constant("a").take(3).toList shouldBe List("a", "a", "a")
    constant(Nil).take(2).toList shouldBe List(Nil, Nil)
  }

  "from(n)" should "generate infinite stream of integers starting from n" in {
    from(0).take(5).toList shouldBe List(0, 1, 2, 3, 4)
    from(1).take(3).toList shouldBe List(1, 2, 3)
  }

  "fibs" should "generate fibonacci sequence" in {
    fibs().take(1).toList shouldBe List(0)
    fibs().take(2).toList shouldBe List(0, 1)
    fibs().take(3).toList shouldBe List(0, 1, 1)
    fibs().take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  "unfold" should "build streams from values produced by a function" in {
    val f = (i: Int) => if (i < 5) Some((i * 2, i + 1)) else None
    unfold(0)(f).take(1).toList shouldBe List(0)
    unfold(0)(f).take(3).toList shouldBe List(0, 2, 4)
    unfold(0)(f).take(5).toList shouldBe List(0, 2, 4, 6, 8)
    unfold(0)(f).take(6).toList shouldBe List(0, 2, 4, 6, 8)
    unfold(0)(f).take(10).toList shouldBe List(0, 2, 4, 6, 8)
  }

  "fibsUnfold" should "generate fibonacci sequence" in {
    fibsUnfold().take(1).toList shouldBe List(0)
    fibsUnfold().take(2).toList shouldBe List(0, 1)
    fibsUnfold().take(3).toList shouldBe List(0, 1, 1)
    fibsUnfold().take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  "fromUnfold(n)" should "generate infinite stream of integers starting from n" in {
    fromUnfold(0).take(5).toList shouldBe List(0, 1, 2, 3, 4)
    fromUnfold(1).take(3).toList shouldBe List(1, 2, 3)
  }

  "onesUnfold" should "generate infinite stream of 1s" in {
    onesUnfold().take(1).toList shouldBe List(1)
    onesUnfold().take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  "mapUnfold" should "apply function to each element in stream" in {
    val f = (i: Int) => i * 2
    Stream.empty.mapUnfold(f).toList shouldBe Stream.empty.toList
    Stream(1).mapUnfold(f).toList shouldBe Stream(2).toList
    Stream(1, 2).mapUnfold(f).toList shouldBe Stream(2, 4).toList
    Stream(1, 2, 3).mapUnfold(f).toList shouldBe Stream(2, 4, 6).toList
  }

  "takeUnfold" should "return first n elements of stream" in {
    Stream.empty.takeUnfold(2).toList shouldBe Stream.empty.toList
    Stream(1).takeUnfold(1).toList shouldBe Stream(1).toList
    Stream(1, 2, 3).takeUnfold(3).toList shouldBe Stream(1, 2, 3).toList
    Stream(1, 2, 3).takeUnfold(2).toList shouldBe Stream(1, 2).toList
    Stream(1, 2).takeUnfold(3).toList shouldBe Stream(1, 2).toList
  }

  "takeWhileUnfold" should "return starting elements that match given predicate" in {
    val p = (i: Int) => i < 5
    Stream.empty.takeWhileUnfold(p).toList shouldBe Stream.empty.toList
    Stream(1).takeWhileUnfold(p).toList shouldBe Stream(1).toList
    Stream(1, 2, 3).takeWhileUnfold(p).toList shouldBe Stream(1, 2, 3).toList
    Stream(1, 2, 5, 6).takeWhileUnfold(p).toList shouldBe Stream(1, 2).toList
    Stream(5).takeWhileUnfold(p).toList shouldBe Stream.empty.toList
    Stream(1, 2, 5, 6, 1).takeWhileUnfold(p).toList shouldBe Stream(1, 2).toList
  }

  "zipWith" should "zip two streams with a function while both have elements" in {
    val f = (a: Int, b: Int) => a + b
    Stream.empty.zipWithUnfold(Stream.empty)(f).toList shouldBe Nil
    Stream.empty.zipWithUnfold(Stream(1))(f).toList shouldBe Nil
    Stream(1).zipWithUnfold(Stream.empty)(f).toList shouldBe Nil
    Stream(1).zipWithUnfold(Stream(2))(f).toList shouldBe List(3)
    Stream(1, 2).zipWithUnfold(Stream(2))(f).toList shouldBe List(3)
    Stream(1).zipWithUnfold(Stream(2, 3))(f).toList shouldBe List(3)
    Stream(1, 2, 3).zipWithUnfold(Stream(2, 3, 4))(f).toList shouldBe List(3, 5, 7)
  }

  "zipAll" should "zip two streams with a function while either has elements" in {
    Stream.empty.zipAll(Stream.empty).toList shouldBe List()
    Stream.empty.zipAll(Stream(1)).toList shouldBe List((None, Some(1)))
    Stream(1).zipAll(Stream.empty).toList shouldBe List((Some(1), None))
    Stream(1).zipAll(Stream(2)).toList shouldBe List((Some(1), Some(2)))
    Stream(1, 2).zipAll(Stream(2, 3)).toList shouldBe List((Some(1), Some(2)), (Some(2), Some(3)))
    Stream(1, 2).zipAll(Stream(2)).toList shouldBe List((Some(1), Some(2)), (Some(2), None))
    Stream(1).zipAll(Stream(2, 3)).toList shouldBe List((Some(1), Some(2)), (None, Some(3)))
  }

}
