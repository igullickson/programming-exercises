package part1

import scala.annotation.tailrec

object Chp3 {

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _::t => t
  }

  def setHead[A](head: A, l: List[A]): List[A] = l match {
    case Nil => head :: Nil
    case _::t => head :: t
  }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = {
    @tailrec
    def helper(l: List[A]): List[A] = l match {
      case Nil => Nil
      case h::t if p(h) => helper(t)
      case _ => l
    }

    helper(l)
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def helper(l: List[A], acc: List[A]): List[A] = l match {
      case Nil | _::Nil => acc
      case h::t => helper(t, acc ::: List(h))
    }

    l match {
      case Nil | _ :: Nil=> Nil
      case h::t => helper(t, List(h))
    }
  }
}
