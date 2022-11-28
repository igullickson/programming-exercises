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
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a,b) => p(a) && b)
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
      foldRight(Stream(a))((a, b) => Stream.cons(a,b))
    }

    def flatMap[AA >: A](f: A => Stream[AA]): Stream[AA] = {
      foldRight(Stream.empty[AA])((a, b) => f(a).foldRight(b)((c,d) => Stream.cons(c, d)))
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

}
