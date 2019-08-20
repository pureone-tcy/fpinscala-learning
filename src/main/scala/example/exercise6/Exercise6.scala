package example.exercise6

object Exercise6 {
  def main(args: Array[String]): Unit = {
    val res = 1 * 0x5DEECE66DL
    println(s"res = ${res}")
  }
}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }

  def randomPair2(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  // Exercise 6-1
  def nonNegativeInt2(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i1, rng1) if i1 <= Int.MaxValue && i1 > Int.MinValue => (i1, rng1)
    case (_, rng2) => nonNegativeInt(rng2)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 6-2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 6-3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // Exercise 6-4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if (count <= 0)
        (l, r)
      else {
        val (i, r1) = r.nextInt
        go(n - 1, i :: l, r1)
      }
    }
    go(count, Nil, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6-5
  def doubleViaMap(s: Rand[Double]): Rand[Double] = {
    map(s)(_ / (Int.MaxValue.toDouble + 1))
  }
  val _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6-6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng1 => {
    val (a, rng2) = ra(rng1)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6-7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }
  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan2(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if(i + (n - 1) - mod >=  0)
      (mod, rng2)
    else
      nonNegativeLessThan2(n)(rng)
  }

  // Exercise 6-8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // Exercise 6-9
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }
}

import State._

case class State[S,+A](run: S => (A, S)) {

  // Exercise 6-10
  def map[B](f: A => B): State[S,B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }
}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  def modify[S](f:S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s,s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}