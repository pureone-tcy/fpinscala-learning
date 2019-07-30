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

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
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


}