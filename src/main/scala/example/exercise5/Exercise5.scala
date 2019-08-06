package example.exercise5

object Exercise5 {
  def main(args: Array[String]): Unit = {
//    val s = Stream(1,2,3,4,5).takeWhileViaUnfold(_ < 3)
    val s = Stream(1,2,3).startsWith(Stream(4,5,6,7))
    println(s)

  }
}

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5-1
//  def toList: List[A] = {
//    @annotation.tailrec
//    def go(s: Stream[A], l: List[A]): List[A] = s match {
//      case Cons(h, t) => go(t(), h() :: l)
//      case _ => l
//    }
//    go(this, List())
//  }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  // Exercise 5-2
  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(n: Int, s: Stream[A], ss: Stream[A]): Stream[A] = s match {
      case Cons(_, _) if n <= 0 => ss
      case Cons(h, t) => go(n - 1, t(), Cons(h, () => ss))
      case _ => Empty
    }

    go(n, this, Stream())
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(n: Int, s: Stream[A]): Stream[A] = s match {
      case Cons(_, t) if n <= 1 => t()
      case Cons(_, t) => go(n - 1, t())
      case _ => Empty
    }

    go(n, this)
  }


  // Exercise 5-3
  def takeWhile(p: A => Boolean): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], ss: Stream[A]): Stream[A] = s match {
      case Cons(h, t) if p(h()) => go(t(), Cons(h, () => ss))
      case Cons(_, t) => go(t(), ss)
      case Empty => ss
    }

    go(this, Stream())
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 5-4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5-5
  import Stream._

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // Exercise 5-6
  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Exercise 5-7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[E >: A](s: Stream[E]): Stream[E] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

//  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5-8
  def constant[B](b: B): Stream[B] = {
    lazy val tail: Stream[B] = Cons(() => b, () => tail)
    tail
  }

  // Exercise 5-9
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  // Exercise 5-10
//  val fibs = {
//    def go(f0: Int, f1: Int): Stream[Int] =
//      cons(f0, go(f1, f0 + f1))
//
//    go(0, 1)
//  }

  // Exercise 5-11
  def unfold[B, S](z: S)(f: S => Option[(B, S)]): Stream[B] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  // Exercise 5-12
//  val fibsViaUnfold = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[B](a: B): Stream[B] = unfold(a)(_ => Some((a, a)))

//  val onesViaUnfold = unfold(1)(_ => Some((1, 1)))

  // Exercise 5-13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), nn) if nn > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty) forAll {
      case (h, h2) => h == h2
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
