package part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import part1.Chp3._

class Chp3Spec extends AnyFlatSpec with should.Matchers {

  "tail" should "return the tail of a list" in {
    tail(Nil) shouldBe Nil
    tail(List(1)) shouldBe Nil
    tail(List(1, 2)) shouldBe List(2)
    tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
  }

  "setHead" should "replace the head of a list" in {
    setHead(1, Nil) shouldBe List(1)
    setHead(1, List(2, 3)) shouldBe List(1, 3)
    setHead(1, List(2, 3, 4)) shouldBe List(1, 3, 4)
  }

  "dropWhile" should "drop items while predicate is true" in {
    val p = (i: Int) => 6 > i

    dropWhile(Nil, p) shouldBe Nil
    dropWhile(List(1), p) shouldBe Nil
    dropWhile(List(7), p) shouldBe List(7)
    dropWhile(List(1, 2, 3, 7, 1), p) shouldBe List(7, 1)
  }

  "init" should "return all but the last element in list" in {
    init(Nil) shouldBe Nil
    init(List(1)) shouldBe Nil
    init(List(1, 2)) shouldBe List(1)
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  "foldRight" should "match my expectation from example in book" in {
    foldRight(List(1, 2, 3), Nil: List[Int])((h, t) => h :: t) shouldBe List(1, 2, 3)
  }

  "foldLeft" should "match my expectation from example in book" in {
    foldLeft(List(1, 2, 3), Nil: List[Int])((t, h) => h :: t) shouldBe List(3, 2, 1)
  }

  "length" should "return length of list" in {
    Chp3.length(Nil) shouldBe 0
    Chp3.length(List(1)) shouldBe 1
    Chp3.length(List(1, 2, 3, 4, 5, 6, 7, 8)) shouldBe 8
  }

  "sumFoldLeft" should "sum ints in list" in {
    sumFoldLeft(Nil) shouldBe 0
    sumFoldLeft(List(1)) shouldBe 1
    sumFoldLeft(List(2, 5, 8, 9)) shouldBe 24
  }

  "productFoldLeft" should "multiply ints in list" in {
    productFoldLeft(Nil) shouldBe 1
    productFoldLeft(List(4)) shouldBe 4
    productFoldLeft(List(2, 5, 8, 9)) shouldBe 720
  }

  "reverse" should "reverse list" in {
    reverse(Nil) shouldBe Nil
    reverse(List(1)) shouldBe List(1)
    reverse(List(1, 2)) shouldBe List(2, 1)
    reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
  }

  "foldLeftWithFoldRight" should "fold as expected" in {
    foldLeftWithFoldRight(List(1, 2, 3, 4, 5), 0)((b, a) => b + a) shouldBe 15
    foldLeftWithFoldRight(List(1, 2, 3), Nil: List[Int])((t, h) => h :: t) shouldBe List(3, 2, 1)
  }

  "foldRightWithFoldLeft" should "fold as expected" in {
    foldRightWithFoldLeft(List(1, 2, 3, 4, 5), 0)((a, b) => a + b) shouldBe 15
    foldRightWithFoldLeft(List(1, 2, 3), Nil: List[Int])((h, t) => h :: t) shouldBe List(1, 2, 3)
  }

  "appendFoldRight" should "append item to end of list" in {
    appendFoldRight(1, Nil) shouldBe List(1)
    appendFoldRight(3, List(4, 5, 7)) shouldBe List(4, 5, 7, 3)
    appendFoldRight(3, List(4, 5, 7, 8)) shouldBe List(4, 5, 7, 8, 3)
  }

  "flatten" should "concatenate list of lists into single list" in {
    flatten(List.empty) shouldBe List.empty
    flatten(List(List.empty)) shouldBe List.empty
    flatten(List(List.empty, List.empty)) shouldBe List.empty

    flatten(List(List(1))) shouldBe List(1)
    flatten(List(List(1,2,3,4))) shouldBe List(1,2,3,4)
    flatten(List(List(1,2), List(3,4,5))) shouldBe List(1,2,3,4,5)

    flatten(List(List.empty, List(1), List.empty, List(2,3,4))) shouldBe List(1,2,3,4)
  }

  "incrementFold" should "add one to each element of list" in {
    incrementFold(List.empty) shouldBe List.empty
    incrementFold(List(1)) shouldBe List(2)
    incrementFold(List(2,3,4)) shouldBe List(3,4,5)
  }

  "doublesToStrings" should "turn each double in list into string" in {
    doublesToStrings(List.empty) shouldBe List.empty
    doublesToStrings(List(1.0)) shouldBe List("1.0")
    doublesToStrings(List(2.3,3.4,4.5)) shouldBe List("2.3", "3.4", "4.5")
  }

  "map" should "apply function to each item in list" in {
    val f = (a: Int) => a * 2
    map(List.empty)(f) shouldBe List.empty
    map(List(1))(f) shouldBe List(2)
    map(List(2,3,4))(f) shouldBe List(4,6,8)
  }

  "filter" should "remove items from list if they don't satisfy predicate" in {
    val f = (a: Int) => a % 2 == 0
    filter(List.empty)(f) shouldBe List.empty
    filter(List(1))(f) shouldBe List.empty
    filter(List(2))(f) shouldBe List(2)
    filter(List(1,2,3,4,5))(f) shouldBe List(2,4)
  }

  "flatMap" should "apply function producing list to each item in list and return a flattened list" in {
    val f = (a: Int) => List(a, a + 10)
    flatMap(List.empty)(f) shouldBe List.empty
    flatMap(List(1))(f) shouldBe List(1, 11)
    flatMap(List(1,2,3))(f) shouldBe List(1, 11, 2, 12, 3, 13)
  }

  "filterWithFlatMap" should "remove items from list if they don't satisfy predicate" in {
    val f = (a: Int) => a % 2 == 0
    filterWithFlatMap(List.empty)(f) shouldBe List.empty
    filterWithFlatMap(List(1))(f) shouldBe List.empty
    filterWithFlatMap(List(2))(f) shouldBe List(2)
    filterWithFlatMap(List(1,2,3,4,5))(f) shouldBe List(2,4)
  }

  "sumLists" should "sum corresponding elements in two lists" in {
    sumLists(List.empty, List.empty) shouldBe List.empty
    sumLists(List(1), List.empty) shouldBe List(1)
    sumLists(List.empty, List(1)) shouldBe List(1)
    sumLists(List(1,2,3), List(2,3,4)) shouldBe List(3,5,7)
    sumLists(List(1,2,3,1,2), List(2,3,4)) shouldBe List(3,5,7,1,2)
  }

  "zipWith" should "apply function to corresponding elements in two lists" in {
    val f = (a: Int, b: Int) => a + b
    zipWith(List.empty, List.empty)(f) shouldBe List.empty
    zipWith(List(1), List.empty)(f) shouldBe List(1)
    zipWith(List.empty, List(1))(f) shouldBe List(1)
    zipWith(List(1,2,3), List(2,3,4))(f) shouldBe List(3,5,7)
    zipWith(List(1,2,3,1,2), List(2,3,4))(f) shouldBe List(3,5,7,1,2)
  }

  "hasSubsequence" should "return whether list contains sublist" in {
    hasSubsequence(Nil, Nil) shouldBe true
    hasSubsequence(List(1), Nil) shouldBe true
    hasSubsequence(Nil, List(1)) shouldBe false
    hasSubsequence(List(1,2,3), List(2,3,4)) shouldBe false
    hasSubsequence(List(1,2,3), List(4,5,6)) shouldBe false
    hasSubsequence(List(1), List(1)) shouldBe true
    hasSubsequence(List(1,2,3), List(1,2,3)) shouldBe true
    hasSubsequence(List(1,2,3), List(3)) shouldBe true
    hasSubsequence(List(1,2,3), List(1,3)) shouldBe false
    hasSubsequence(List(1,2,3), List(1,2)) shouldBe true
    hasSubsequence(List(1), List(1,2)) shouldBe false
  }

  "size" should "return number of leaves and branches in a tree" in {
    Chp3.size(Leaf(1)) shouldBe 1
    Chp3.size(Branch(Leaf(1), Leaf(2))) shouldBe 3
    Chp3.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 5
    Chp3.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))) shouldBe 7
  }

  "sizeBreadth" should "return number of leaves and branches in a tree" in {
    Chp3.sizeBreadth(Leaf(1)) shouldBe 1
    Chp3.sizeBreadth(Branch(Leaf(1), Leaf(2))) shouldBe 3
    Chp3.sizeBreadth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 5
    Chp3.sizeBreadth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))) shouldBe 7
  }

  "maximum" should "return the largest int in a tree" in {
    maximum(Leaf(1)) shouldBe 1
    maximum(Branch(Leaf(1), Leaf(2))) shouldBe 2
    maximum(Branch(Leaf(2), Leaf(1))) shouldBe 2
    maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    maximum(Branch(Branch(Leaf(3), Leaf(2)), Leaf(1))) shouldBe 3
    maximum(Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))) shouldBe 3
    maximum(Branch(Branch(Leaf(4), Leaf(2)), Branch(Leaf(1), Leaf(2)))) shouldBe 4
    maximum(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(1), Leaf(2)))) shouldBe 4
    maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(2)))) shouldBe 4
    maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(4)))) shouldBe 4
  }

  "maxBreadth" should "return the largest int in a tree" in {
    maxBreadth(Leaf(1)) shouldBe 1
    maxBreadth(Branch(Leaf(1), Leaf(2))) shouldBe 2
    maxBreadth(Branch(Leaf(2), Leaf(1))) shouldBe 2
    maxBreadth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    maxBreadth(Branch(Branch(Leaf(3), Leaf(2)), Leaf(1))) shouldBe 3
    maxBreadth(Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))) shouldBe 3
    maxBreadth(Branch(Branch(Leaf(4), Leaf(2)), Branch(Leaf(1), Leaf(2)))) shouldBe 4
    maxBreadth(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(1), Leaf(2)))) shouldBe 4
    maxBreadth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(2)))) shouldBe 4
    maxBreadth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(4)))) shouldBe 4
  }
  
  "depth" should "return the max path length from root to any leaf" in {
    depth(Leaf(1)) shouldBe 1
    depth(Branch(Leaf(1), Leaf(2))) shouldBe 2
    depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    depth(Branch(Leaf(3), Branch(Leaf(1), Leaf(2)))) shouldBe 3
    depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))) shouldBe 3
    depth(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2)), Branch(Leaf(1), Leaf(2)))) shouldBe 4
  }
  
  "map" should "apply function to each element in tree" in {
    val f = (i: Int) => i * 2
    map(Leaf(1), f) shouldBe Leaf(2)
    map(Branch(Leaf(1), Leaf(2)), f) shouldBe Branch(Leaf(2), Leaf(4))
    map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), f) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
    map(Branch(Leaf(3), Branch(Leaf(1), Leaf(2))), f) shouldBe Branch(Leaf(6), Branch(Leaf(2), Leaf(4)))
    map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))), f) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(2), Leaf(4)))
    map(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2)), Branch(Leaf(1), Leaf(2))), f) shouldBe Branch(Branch(Branch(Leaf(2), Leaf(4)), Leaf(4)), Branch(Leaf(2), Leaf(4)))
  }
  
  "size with fold" should "return number of leaves and branches in a tree" in {
    val f = (a: Int, b: Int) => a + b + 1
    val g = (_: Int) => 1

    fold(Leaf(1), f, g) shouldBe 1
    fold(Branch(Leaf(1), Leaf(2)), f, g) shouldBe 3
    fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), f, g) shouldBe 5
    fold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))), f, g) shouldBe 7
  }

  "maximum with fold" should "return the largest int in a tree" in {
    val f = (a: Int, b: Int) => a max b
    val g = (a: Int) => a

    fold(Leaf(1), f, g) shouldBe 1
    fold(Branch(Leaf(1), Leaf(2)), f, g) shouldBe 2
    fold(Branch(Leaf(2), Leaf(1)), f, g) shouldBe 2
    fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), f, g) shouldBe 3
    fold(Branch(Branch(Leaf(3), Leaf(2)), Leaf(1)), f, g) shouldBe 3
    fold(Branch(Branch(Leaf(2), Leaf(3)), Leaf(1)), f, g) shouldBe 3
    fold(Branch(Branch(Leaf(4), Leaf(2)), Branch(Leaf(1), Leaf(2))), f, g) shouldBe 4
    fold(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(1), Leaf(2))), f, g) shouldBe 4
    fold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(2))), f, g) shouldBe 4
    fold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(4))), f, g) shouldBe 4
  }

  "depth with fold" should "return the max path length from root to any leaf" in {
    val f = (a: Int, b: Int) => (a max b) + 1
    val g = (_: Int) => 1

    fold(Leaf(1), f, g) shouldBe 1
    fold(Branch(Leaf(1), Leaf(2)), f, g) shouldBe 2
    fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), f, g) shouldBe 3
    fold(Branch(Leaf(3), Branch(Leaf(1), Leaf(2))), f, g) shouldBe 3
    fold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))), f, g) shouldBe 3
    fold(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2)), Branch(Leaf(1), Leaf(2))), f, g) shouldBe 4
  }

  "map with fold" should "apply function to each element in tree" in {
    val f = (a: Tree[Int], b: Tree[Int]) => Branch(a, b)
    val g = (a: Int) => Leaf(a * 2)

    fold(Leaf(1), f, g) shouldBe Leaf(2)
    fold(Branch(Leaf(1), Leaf(2)), f, g) shouldBe Branch(Leaf(2), Leaf(4))
    fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), f, g) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
    fold(Branch(Leaf(3), Branch(Leaf(1), Leaf(2))), f, g) shouldBe Branch(Leaf(6), Branch(Leaf(2), Leaf(4)))
    fold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))), f, g) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(2), Leaf(4)))
    fold(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2)), Branch(Leaf(1), Leaf(2))), f, g) shouldBe Branch(Branch(Branch(Leaf(2), Leaf(4)), Leaf(4)), Branch(Leaf(2), Leaf(4)))
  }
  
}
