package state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), r)
  }
  // 6.3 

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def gen(n: Int, r: RNG, l: List[Int]): (List[Int], RNG) =
      if (n == 0) (l, r)
      else {
        val (i, nr) = r.nextInt
        gen(n - 1, nr, i :: l)
      }
    gen(count, rng, List())
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  //6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)
  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      {
        @tailrec
        def go(l: List[Rand[A]], r: RNG, rt: List[A]): (List[A], RNG) =
          l match {
            case h :: t => {
              val (s, nr) = h(rng)
              go(t, nr, s :: rt)
            }
            case Nil => (rt, r)
          }
        go(fs, rng, List())
      }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence2(List.fill(count)(int))
  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nr) = f(rng)
      g(a)(nr)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  // 6.9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => {
      flatMap(rb)(b => {
        unit(f(a, b))
      })
    })
}