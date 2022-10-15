package part1

import scala.annotation.tailrec

object Chp2 {

  // 1  2  3  4  5  6  7  8   9   10  11
  // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55

  def fib(n: Int): Int = n match {
      case 1 => 0
      case 2 => 1
      case _ => fib(n - 1) + fib(n - 2)
  }

  def fibR(n: Int): Int = {
    @tailrec
    def helper(last: Int, current: Int, iteration: Int): Int = {
      if (iteration >= n) current
      else helper(current, last + current, iteration + 1)
    }

    n match {
      case 1 => 0
      case 2 => 1
      case _ => helper(1, 1, 3)
    }
  }

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

  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def helper(head: A, remaining: List[A]): Boolean = {
      remaining match {
        case Nil => true
        case h::t if ordered(head, h) => helper(h, t)
        case _ => false
      }
    }

    as match {
      case h::t => helper(h, t)
      case _ => true
    }
  }

  def isSortedArray[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def helper(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else helper(i + 1)
    }

    if (as.length < 2) true
    else helper(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    (a: A) => f(a, _)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }



}
