package example.exercise5

object Exercise5 {
  def main(args: Array[String]): Unit = {
    //    val s = Stream(1,2,3,4,5,6,7,8,9,10).takeWhile2(_ == 5)
    //    val s = Stream(1,2,3,4,5,6,7,8,9,10).headOption2
    //    import Stream._
    val s = Stream(1, 2, 3, 4, 5).map(_ * 2)
    //    val s = Stream(1,2,3,4,5).flatMap(a => cons(a * 2, empty))
    //    val s = Stream(1,2,3,4,5).append(Stream(6,7,8,9,10))

    println(Stream().fibsViaUnfold.toList)
  }
}

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5-1
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: l)
      case _ => l
    }
    go(this, List())
  }
//  def toList: List[A] = this match {
//    case Empty => Nil
//    case Cons(h, t) => h() :: t().toList
//  }

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
      case Cons(h, t) if p(h())  => go(t(), Cons(h, () => ss))
      case Cons(_, t) => go(t(), ss)
      case Empty => ss
    }
    go(this, Stream())
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h( )) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => println(h()); f(h(), t().foldRight(z)(f))
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
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  // Exercise 5-6
  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Exercise 5-7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)
  def append[E >: A](s: Stream[E]): Stream[E] =
    foldRight(s)((a, b) => cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  // Exercise 5-8
  def constant[B](a: B): Stream[B] = {
    lazy val tail: Stream[B] = Cons(() => a, () => tail)
    tail
  }

  // Exercise 5-9
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  // Exercise 5-10
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  // Exercise 5-11
  def unfold[B, S](z: S)(f: S => Option[(B, S)]): Stream[B] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  // Exercise 5-12
  val fibsViaUnfold = unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

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
