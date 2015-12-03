package datastructure

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => t
  }

  def setHead[A](as: List[A], value: A): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => Cons(value, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def go(next: List[A], cnt: Int): List[A] =
      if (cnt == n || next == Nil) next
      else go(tail(next), cnt + 1)
    go(l, 0)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil        => Nil
    case Cons(h, t) => if (f(h)) dropWhile(tail(l), f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def fold[A, B](l: List[A], b: B, fx: (B, A) => B, acc: B): B = l match {
      case Nil          => b
      case Cons(h, Nil) => fx(acc, h)
      case Cons(h, t)   => fold(t, b, fx, fx(acc, h))
    }
    fold(as, z, f, z)
  }
  def length[A](as: List[A]): Int = foldRight(as, 0)((a: A, b: Int) => b + 1)

  def append[A](l: List[A], l2: List[A]): List[A] =
    l match {
      case Nil        => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }

  def append[A](as: List[A], v: A): List[A] = {
    as match {
      case Nil        => Cons(v, Nil)
      case Cons(h, t) => Cons(h, append[A](t, v))
    }
  }

  def appendViaFoldLeft[A](l: List[A], v: A): List[A] =
    foldRight(l, List[A]())((el: A, r: List[A]) => if (r == Nil) Cons(el, Cons(v, Nil)) else Cons(el, r))

  def last[A](as: List[A]): List[A] = as match {
    case Nil          => Nil
    case Cons(h, Nil) => Cons(h, Nil)
    case Cons(h, t)   => last(t)
  }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, el) => Cons(el, acc))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def plusOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  def convertDoubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((a, b) => Cons(a.toString(), b))

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil        => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def map2[A, B](l: List[A])(f: A => B): List[B] = {
    val buffer = new ListBuffer[B]()
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => {
        buffer += f(h)
        go(t)
      }
    }
    go(l)
    List(buffer.toList: _*)
  }

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buffer = new ListBuffer[A]()
    def go(as: List[A]): Unit = as match {
      case Nil => ()
      case Cons(h, t) => {
        if (!f(h)) buffer += h
        go(t)
      }
    }
    go(l)
    List(buffer.toList: _*)
  }

  def filterViaFoldRight[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil        => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  // 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(v => if (f(v)) Nil else List(v))    
}