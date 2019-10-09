package com.github.pureone.laziness

trait Stream[+A] {

  import Stream._

  def toList: List[A] = {
    def go(l: Stream[A]): List[A] = l match {
      case Empty       => Nil
      case Cons(x, xs) => x() :: go(xs())
    }
    go(this)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
