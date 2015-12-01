package datastructure

import scala.annotation.tailrec

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

  def append[A](as: List[A], v: A): List[A] = {
    as match {
      case Nil        => Cons(v, Nil)
      case Cons(h, t) => Cons(h, append[A](t, v))
    }
  }

  def last[A](as: List[A]): List[A] = as match {
    case Nil          => Nil
    case Cons(h, Nil) => Cons(h, Nil)
    case Cons(h, t)   => last(t)
  }
  
  def reverse[A](as: List[A]): List[A] = 
    foldLeft(as, List[A]())((acc,el) => Cons(el, acc))

}