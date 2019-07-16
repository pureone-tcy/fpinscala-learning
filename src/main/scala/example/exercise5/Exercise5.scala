package example.exercise5

object Exercise5 {
  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3,4,5,6,7,8,9,10).exists2(_ % 2 == 0)
    println(s)
  }
}

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.1
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

  // Exercise 5.2
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

  // Exercise 5.3
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
    case  _=> z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

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
