package state

case class State[S, +A](run: S => (A, S)) {
  // 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map { b => f(a, b) })
    
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object State {
  def unit[S, A](a: A): State[S, A] = State { (st: S) => (a, st) }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, l: List[State[S, A]], acc: List[A]): (List[A], S) = {
      l match {
        case Nil => (acc, s)
        case h :: t => h.run(s) match {
          case (a, s1) => go(s1, t, a :: acc)
        }
      }
    }
    State((s: S) => go(s, fs, List()))
  }
}