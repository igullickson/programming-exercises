package part1

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Chp3 {

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: t => t
  }

  def setHead[A](head: A, l: List[A]): List[A] = l match {
    case Nil => head :: Nil
    case _ :: t => head :: t
  }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = {
    @tailrec
    def helper(l: List[A]): List[A] = l match {
      case Nil => Nil
      case h :: t if p(h) => helper(t)
      case _ => l
    }

    helper(l)
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def helper(l: List[A], acc: List[A]): List[A] = l match {
      case Nil | _ :: Nil => acc
      case h :: t => helper(t, acc ::: List(h))
    }

    l match {
      case Nil | _ :: Nil => Nil
      case h :: t => helper(t, List(h))
    }
  }

  // folds list right to left
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, b) => 1 + b)
  }


  /*
  sumFoldLeft(List(1,2,3,4,5))
  foldLeft(List(2,3,4,5), (0 + 1))
  ...
  (((((0 + 1) + 2) + 3) + 4) + 5)
 */
  // folds list left to right
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }

  def sumFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)((b, a) => b + a)

  def productFoldLeft(l: List[Int]): Int =
    foldLeft(l, 1)((a, b) => a * b)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => a :: b)

  def foldLeftWithFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a, b) => f(b, a))

  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b, a) => f(a, b))

  def appendFoldRight[A](item: A, l: List[A]): List[A] =
    foldRight(l, List(item))((a, b) => a :: b)

  def flatten[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, List.empty[A])((a, b) => a ::: b)
  }

  def incrementFold(ls: List[Int]): List[Int] = {
    foldRight(ls, List.empty[Int])((a, b) => (a + 1) :: b)
  }

  def doublesToStrings(ls: List[Double]): List[String] = {
    foldRight(ls, List.empty[String])((a, b) => a.toString :: b)
  }

  def map[A, B](ls: List[A])(f: A => B): List[B] = {
    foldRight(ls, List.empty[B])((a, b) => f(a) :: b)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List.empty[A])((a, b) => if (f(a)) a :: b else b)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List.empty[B])((a, b) => f(a) ::: b)
  }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else List.empty)
  }

  def sumLists(ls: List[Int], ls2: List[Int]): List[Int] = {
    @tailrec
    def helper(ls: List[Int], ls2: List[Int], acc: List[Int]): List[Int] = {
      (ls, ls2) match {
        case (Nil, Nil) => acc
        case (Nil, _ :: _) => acc ::: ls2
        case (_ :: _, Nil) => acc ::: ls
        case (h :: t, h2 :: t2) => helper(t, t2, acc ::: List(h + h2))
      }
    }

    helper(ls, ls2, List.empty)
  }

  def zipWith[A](ls: List[A], ls2: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def helper(ls: List[A], ls2: List[A], acc: List[A]): List[A] = {
      (ls, ls2) match {
        case (Nil, Nil) => acc
        case (Nil, _ :: _) => acc ::: ls2
        case (_ :: _, Nil) => acc ::: ls
        case (h :: t, h2 :: t2) => helper(t, t2, acc ::: List(f(h, h2)))
      }
    }

    helper(ls, ls2, List.empty)
  }

  @tailrec
  def hasSubsequence[A](ls: List[A], sub: List[A]): Boolean = {
    @tailrec
    def helper(ls: List[A], sub: List[A]): Boolean = {
      (ls, sub) match {
        case (_, Nil) => true
        case (ah :: at, bh :: bt) if ah == bh => helper(at, bt)
        case _ => false
      }
    }

    (ls, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (h :: t, sh :: st) =>
        if (h == sh) {
          if (helper(t, st)) true
          else hasSubsequence(t, sub)
        }
        else hasSubsequence(t, sub)
    }
  }

  // 3.5 Trees
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = {
    t match {
      case _: Leaf[A] => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def sizeBreadth[A](t: Tree[A]): Int = {
    @tailrec
    def helper(q: Queue[Tree[A]], acc: Int): Int = {
      q.dequeueOption match {
        case Some((h, tail)) =>
          h match {
            case Leaf(_) => helper(tail, acc + 1)
            case Branch(l, r) => helper(tail ++: Queue(l, r), acc + 1)
          }
        case None => acc
      }
    }

    helper(Queue(t), 0)
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(i) => i
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def maxBreadth[A](t: Tree[Int]): Int = {
    @tailrec
    def helper(q: Queue[Tree[Int]], acc: Int): Int = {
      q.dequeueOption match {
        case Some((Leaf(i), tail)) => helper(tail, acc max i)
        case Some((Branch(l, r), tail)) => helper(tail ++: Queue(l, r), acc)
        case None => acc
      }
    }

    helper(Queue(t), 0)
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }
  }

  /*

    def size[A](t: Tree[A]): Int = {
    t match {
      case _: Leaf[A] => 1
      case Branch(l, r) => +(size(l), size(r)) + 1
    }
  }


  f = (a: B,b: B) => a + b + 1

    def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(i) => i
      case Branch(l, r) => max(maximum(l), maximum(r))
    }
  }

  f = (a,b) => a max b

     def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => max(fold(l)), fold(r))
    }
  }

  f = (a,b) => (a max b) + 1


  def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
    t match {
      case Leaf(i) => Leaf(f(i))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }
  }

  f = (a,b) => Branch(a, b)
   */

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
    t match {
      case Leaf(i) => Leaf(f(i))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }
  }

  /**
   * @param f a function for values in a branch
   * @param g a function for values in a leaf
   */
  def fold[A, B](t: Tree[A], f: (B, B) => B, g: A => B): B = {
    t match {
      case Leaf(a) => g(a)
      case Branch(l, r) => f(fold(l, f, g), fold(r, f, g))
    }
  }

}
