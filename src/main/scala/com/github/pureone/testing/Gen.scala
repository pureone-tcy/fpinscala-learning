package com.github.pureone.testing

import com.github.pureone.laziness._
import com.github.pureone.state._
import com.github.pureone.testing.Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x               => x
    }
  }

  def ||(p: Prop) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x                 => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x               => x
    }
  }
}

object Prop {
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
//  type Result = Option[(FailedCase, SuccessCount)]

  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

//  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
//    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
//      case (a, i) => try {
//        if (f(a)) Passed else Falsified(a.toString, i)
//      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
//    }.find(_.isFalsified).getOrElse(Passed)
//  }
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def liftOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (a => this.liftOfN(a))

  def liftOfN(size: Int): Gen[List[A]] =
    Gen.liftOfN(size, this)
}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def liftOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

}
