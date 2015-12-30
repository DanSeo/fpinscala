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
    case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
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

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def existViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }
  // 5.4 
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _          => false
  }

  def forAllViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) && b)

  // 5.5. 
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  // 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, b) => b match {
      case None    => Some(a)
      case Some(h) => Some(h)
    })
  def headOption2ViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7  None Strict: appendViaFoldRight
  // def mapViaFoldRight filterViaFoldRight appendViaFoldRight flatMapViaFoldRight

  def mapViaFoldRight[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((a, b) => Stream.cons(f(a), b))

  def filterViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def appendViaFoldRight[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a) appendViaFoldRight b)
  // 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this)(s => s match {
    case Cons(h, t) => Some((f(h()), t()))
    case _          => None
  })
  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, n))({
    case (Cons(h, t), 1)            => Some(h(), (Empty, 0))
    case (Cons(h, t), num) if n > 1 => Some((h(), (t(), num - 1)))
    case _                          => None
  })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _                    => None
  }
  def zipWith[B, C](r: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, r)) {
    case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
    case _                          => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)) {
    case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
    case (Cons(h, t), Empty)        => Some((Some(h()), None), (t(), Empty))
    case (Empty, Cons(h2, t2))      => Some((None, Some(h2())), (Empty, t2()))
    case (Empty, Empty)             => None
  }

  // 5.14
  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(a => !a._2.isEmpty).forAll {
    case (h1, h2) => h1 == h2
  }

  // 5.15 tails 
  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Empty => None
    case s     => Some((s, s drop 1))
  } appendViaFoldRight Stream(Empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  // 5.16 
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val b2 = b
      val r = f(a, b2._1)
      (r, Stream.cons(r, b2._2))
    })._2
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
  // 5.8 문제 풀이 
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constantViaSmartcons[A](a: A): Stream[A] =
    cons(a, constantViaSmartcons(a))

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  // 5.10
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      lazy val tail: Stream[Int] = cons(f0, go(f1, f0 + f1))
      tail
    }
    go(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => Empty
    }

  // 5.12
  val fibsViaUnfold = unfold[Int, (Int, Int)]((0, 1)) { s => Some((s._1, (s._2, s._1 + s._2))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n) { s => Some((s, s + 1)) }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a) { s => Some((s, s)) }

  val onesViaViaUnfold = unfold(1) { s => Some((1, 1)) }

}
