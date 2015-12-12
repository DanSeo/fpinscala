package errorhandling

sealed trait Option[+A] extends Any {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some[B](f(a))
    case _       => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }
  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap { x => b.map { y => f(x, y) } }

  def mapViaForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // 4.4 it's hard. 
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => h.flatMap { x => sequence(t).map(x :: _) }
  }
  // 4.5 
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil    => Some(Nil)
    case h :: t => f(h).flatMap { x => traverse(t)(f).map(x :: _) }
  }
  
  def traverseViaMap[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverseViaMap(t)(f))(_ :: _)
  }
    

}
object Math {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
  // 4.2 
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { m => mean(xs.map(x => math.pow(x - m, 2))) }

}

