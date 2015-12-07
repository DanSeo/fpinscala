package errorhandling

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some[B](f(a))
    case _ => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
  
  def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None) 
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]