package part1

import scala.annotation.tailrec

object Chp5 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toList: List[A] = {
      @tailrec
      def helper(s: Stream[A], acc: List[A]): List[A] = {
        s match {
          case Empty => acc
          case Cons(h, t) => helper(t(), acc ::: List(h()))
        }
      }

      helper(this, Nil)
    }

    def toListFast: List[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      @tailrec
      def helper(s: Stream[A]): List[A] = {
        s match {
          case Cons(h, t) =>
            buf += h()
            helper(t())
          case Empty => buf.toList
        }
      }

      helper(this)
    }

    def take(n: Int): Stream[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      @tailrec
      def helper(s: Stream[A]): Stream[A] = {
        (s, buf.length) match {
          case (Cons(h, t), len) if len < n =>
            buf += h()
            helper(t())
          case _ => Stream.apply(buf.toList: _*)
        }
      }

      helper(this)
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      @tailrec
      def helper(s: Stream[A]): Stream[A] = {
        s match {
          case Empty => Stream.apply(buf.toList: _*)
          case Cons(h, t) =>
            val head = h()
            if (p(head)) {
              buf += head
              helper(t())
            }
            else Stream.apply(buf.toList: _*)
        }
      }

      helper(this)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)
    }

    def headOptionFoldRight: Option[A] = {
      foldRight(Option.empty[A])((a, _) => Some(a))
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)
    }

    def append[AA >: A](a: => AA): Stream[AA] = {
      foldRight(Stream(a))((a, b) => Stream.cons(a, b))
    }

    def flatMap[AA >: A](f: A => Stream[AA]): Stream[AA] = {
      foldRight(Stream.empty[AA])((a, b) => f(a).foldRight(b)((c, d) => Stream.cons(c, d)))
    }

    def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

    def takeUnfold(n: Int): Stream[A] = unfold((this, 0)) {
      case (_, i) if i == n => None
      case (Empty, _) => None
      case (Cons(h, t), i) => Some((h(), (t(), i + 1)))
    }

    def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        val hh = h()
        if (p(hh)) Some((hh, t()))
        else None
    }

    def zipWithUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
      case (Empty, _) | (_, Empty) => None
      case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h,t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
    }

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _ *))
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def helper(x: Int, y: Int): Stream[Int] = Stream.cons(x + y, helper(y, x + y))

    Stream.cons(0, Stream.cons(1, helper(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(s: S): Stream[A] = {
      f(s) match {
        case Some((a, next)) => Stream.cons(a, go(next))
        case None => Stream.empty[A]
      }
    }

    go(z)
  }

  def fibsUnfold(): Stream[Int] = {
    val f = (t: (Int, Int)) => Some((t._1 + t._2, (t._2, t._1 + t._2)))
    Stream.cons(0, Stream.cons(1, unfold((0, 1))(f)))
  }

  def fromUnfold(n: Int): Stream[Int] = unfold(n)((x: Int) => Some((x, x + 1)))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)((a: A) => Some((a, a)))

  def onesUnfold(): Stream[Int] = unfold(1)(_ => Some((1, 1)))

}
