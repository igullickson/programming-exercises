package part1

import scala.annotation.tailrec
import scala.collection.mutable

object Chp2 {

  // 1  2  3  4  5  6  7  8   9   10  11
  // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55

  def fib(n: Int): Int = n match {
      case 1 => 0
      case 2 => 1
      case _ => fib(n - 1) + fib(n - 2)
  }

//  def fibTR(n: Int): Int = {
//
//    @tailrec
//    def go(n: Int, acc: Int): Int = n match {
//      case 1 => 0
//      case 2 => 1
//      case _ => acc + go(n - 1, 0)
//    }
//
//    go(n, 0)
//  }

  def fibI(n: Int): Int = {
    n match {
      case 1 => 0
      case 2 => 1
      case _ =>
        var x = 0
        var y = 1
        var z = x + y

        (3 until n).foreach { _ =>
          x = y
          y = z
          z = x + y
        }

        z
    }
  }


//  def fibMemoized(n: Int): Int = {
//    val map = mutable.Map(1 -> 0, 2 -> 1)
//
//    def go(n: Int): Int = {
//      map.getOrElse(n, {
//        val result = go(n - 1) + go(n - 2)
//        map.addOne(n -> result)
//        result
//      })
//    }
//
//    go(n)
//  }

}
