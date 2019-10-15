package com.github.pureone.testing

import java.util.concurrent.{ExecutorService, Executors}

import com.github.pureone.laziness._
import com.github.pureone.state._
import com.github.pureone.parallelism.Par._
import com.github.pureone.parallelism._
import Gen._
import Prop._


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }
}

object Prop {
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

//  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a))
          Passed
        else
          Falsified(a.toString, i)
      } catch {
        case e: Exception =>
          Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop {
      (_, n, rng) => f(n, rng)
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] = Stream
          .from(0)
          .take((n min max) + 1)
          .map(i => forAll(g(i))(f))
      val prop: Prop = props
        .map(p => Prop {
          (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  def check(p: => Boolean): Prop =
    forAll(Gen.unit(()))(_ => p)

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) ->.75,
    Gen.unit(Executors.newCachedThreadPool) ->.25)
//  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
//    forAll(S.map2(g)((_,_))) { case (s, a) => f(a)(s).get }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
//    forAll(S ** g) { case (s, a) => f(a)(s).get }
    forAll(S ** g) {case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val p4 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0,10) map Par.unit
  val p5 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"

  val isEven = (i: Int) => i % 2 == 0

}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap(a => this.listOfN(a))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_, _))

}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  val smallInt: Gen[Int] = Gen.choose(-10,10)
  val maxProp = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))
  val maxProp1 = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  val sortedProp: Prop = forAll(listOf(smallInt)) { l: List[Int] =>
    val ls = l.sorted
    l.isEmpty ||
      ls.tail.isEmpty ||
      !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  val pint2: Gen[Par[Int]] = choose(-100,100)
    .listOfN(choose(0,20))
    .map(l =>
      l.foldLeft(Par.unit(0))((p,i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))(_ + _)
        }))
}

case class SGen[+A](g: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen{ g(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { f(_).g(n) }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))

}


