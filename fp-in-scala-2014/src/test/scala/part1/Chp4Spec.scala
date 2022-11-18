package part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import part1.Chp4._
import org.scalatest.prop.TableDrivenPropertyChecks._

class Chp4Spec extends AnyFlatSpec with should.Matchers {

  "Option.map" should "apply function if option is not none" in {
    None.map(_ => 1) shouldBe None
    Some(1).map(_ + 1) shouldBe Some(2)
  }

  "Option.flatMap" should "apply f to the option if not none" in {
    None.flatMap(_ => Some(1)) shouldBe None
    Some(1).flatMap(a => Some(a + 1)) shouldBe Some(2)
  }

  "Option.getOrElse" should "get option or return default" in {
    None.getOrElse(1) shouldBe 1
    Some(2).getOrElse(1) shouldBe 2
  }

  "Option.orElse" should "get option or evaluate default option" in {
    val f = (a: Int) => Some(a)
    None.orElse(f(1)) shouldBe Some(1)
    Some(2).orElse(f(1)) shouldBe Some(1)
  }

  "Option.filter" should "convert to None if value doesn't satisfy function" in {
    None.filter(_ => true) shouldBe None
    None.filter(_ => false) shouldBe None
    Some(1).filter(_ > 1) shouldBe None
    Some(2).filter(_ > 1) shouldBe Some(2)
  }

  "variance" should "calculate variance for a sequence of doubles" in {
    variance(List.empty) shouldBe None
    variance(List(1,2,3,4,5,6)) shouldBe Some(17.5/6)
  }

  "map2" should "combine two Option values into a binary function" in {
    val options = Table(
      ("a", "b", "expectedResult"),
      (None, None, None),
      (None, Some(1), None),
      (Some(1), None, None),
      (Some(1), Some(1), Some(2))
    )

    val f = (a: Int, b: Int) => a + b

    forAll(options) { (a, b, expectedResult) =>
      map2(a,b)(f) shouldBe expectedResult
    }
  }

  "sequence" should "combine list of options into one option containing list of all some values, or none if any are none" in {
    Option.sequence(Nil) shouldBe Some(Nil)
    Option.sequence(List(Some(1))) shouldBe Some(List(1))
    Option.sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))

    Option.sequence(List(None, Some(1))) shouldBe None
    Option.sequence(List(Some(1), None)) shouldBe None
    Option.sequence(List(Some(1), Some(2), None)) shouldBe None
    Option.sequence(List(Some(1), None, Some(2), Some(3))) shouldBe None
    Option.sequence(List(Some(1), Some(2), Some(3), None)) shouldBe None
    Option.sequence(List(None, Some(1), Some(2), Some(3), None)) shouldBe None
  }

  "traverse" should "map function to every value in list and sequence results" in {
    val f = (i: Int) => if (i < 0) None else Some(i * 2)

    Option.traverse(Nil)(f) shouldBe Some(Nil)
    Option.traverse(List(1))(f) shouldBe Some(List(2))
    Option.traverse(List(1,2))(f) shouldBe Some(List(2, 4))
    Option.traverse(List(1,2,3))(f) shouldBe Some(List(2, 4, 6))

    Option.traverse(List(-1, 1))(f) shouldBe None
    Option.traverse(List(1,2,-1))(f) shouldBe None
    Option.traverse(List(1,-1,2,3))(f) shouldBe None
  }

}
