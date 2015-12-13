package errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(r) => f(r)
    case Left(e)  => Left(e)
  }
  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(r) => Right(r)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil    => Right(Nil)
    case h :: t => h.flatMap { x => sequence(t).map { x :: _ } }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil    => Right(Nil)
    case h :: t => f(h).flatMap { x => traverse(t)(f).map { x :: _ } }
  }

  //def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]]

  //def traverseViaFoldRight[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]
}