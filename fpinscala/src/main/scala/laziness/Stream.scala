package laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  // 5.1
  def toListRecursive: List[A] = {
    def go(l: Stream[A]): List[A] = l match {
      case Cons(h, t) => h() :: go(t())
      case _          => List()
    }
    go(this)
  }
  def toListRecursive2: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive2
    case _          => List()
  }
  def toList: List[A] = {
    @tailrec
    def go(l: Stream[A], acc: List[A]): List[A] = l match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc
    }
    go(this, List()).reverse
  }
  // TOOD ListBuffer 를 사용하는 방법을 쓰자

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 0 => Stream.cons(h(), Empty)
    case _                    => Empty
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => go(t(), n - 1)
      case _                   => s
    }
    go(this, n)
  }

  // 5.3 
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _                    => Empty
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
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}