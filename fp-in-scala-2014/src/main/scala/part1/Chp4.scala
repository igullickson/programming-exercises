package part1

import scala.annotation.tailrec

object Chp4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(a) => Some(f(a))
        case _ => None
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this match {
        case Some(a) => f(a)
        case _ => None
      }
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(a) => a
        case _ => default
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case Some(_) => this
        case _ => ob
      }
    }

    def filter(f: A => Boolean): Option[A] = {
      this match {
        case Some(a) if f(a) => this
        case _ => None
      }
    }
  }

  object Option {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      @tailrec
      def helper(as: List[Option[A]], acc: List[A]): Option[List[A]] = {
        as match {
          case None :: _ => None
          case Some(v) :: t => helper(t, acc ::: List(v))
          case Nil => Some(acc)
        }
      }

      helper(a, Nil)
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      @tailrec
      def helper(as: List[A], acc: List[B]): Option[List[B]] = {
        as match {
          case h :: t =>
            f(h) match {
              case Some(b) => helper(t, acc ::: List(b))
              case None => None
            }
          case Nil => Some(acc)
        }
      }

      helper(a, Nil)
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      val mean = xs.sum / xs.length
      Some(xs.foldLeft(0.0)((acc, a) => acc + Math.pow(a - mean, 2)) / xs.length)
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Right(a) => Right(f(a))
        case l@Left(_) => l
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(a) => f(a)
        case l@Left(_) => l
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case r@Right(_) => r
        case _ => b
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      (this, b) match {
        case (Right(a), Right(b)) => Right(f(a, b))
        case (l@Left(_), _) => l
        case (_, l@Left(_)) => l
      }
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      @tailrec
      def helper(ls: List[Either[E, A]], acc: List[A]): Either[E, List[A]] = {
        ls match {
          case h :: t =>
            h match {
              case l@Left(_) => l
              case Right(a) => helper(t, acc ::: List(a))
            }
          case _ => Right(acc)
        }
      }

      helper(es, Nil)
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      def helper(ls: List[A], acc: List[B]): Either[E, List[B]] = {
        ls match {
          case h::t =>
            f(h) match {
              case l@Left(_) => l
              case Right(a) => helper(t, acc ::: List(a))
            }
          case _ => Right(acc)
        }
      }

      helper(as, Nil)
    }
  }

}
