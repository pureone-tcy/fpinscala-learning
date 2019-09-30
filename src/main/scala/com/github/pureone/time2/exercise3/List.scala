package com.github.pureone.time2.exercise3

sealed trait List[+A] {
  def tail: List[A]
  def setHead[B >: A](a: B): List[B] = Cons(a, this.tail)

  def drop[B >: A](n: Int, l: List[B] = this): List[B] = l match {
    case Nil                 => Nil
    case Cons(_, t) if n > 0 => drop(n - 1, t)
    case _                   => l
  }

  def dropWhile[B >: A](l: List[B])(f: B => Boolean): List[B] = this match {
    case Nil                => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _                  => l
  }

}
case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = Nil
  override def setHead[B >: Nothing](a: B): List[B] = Nil
}
case class Cons[+A](h: A, t: List[A]) extends List[A] {
  override def tail: List[A] = t
}

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil        => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t)   => h * product(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, n) => n + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length3[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

  // TODO 12

//  def reverse[A](as: List[A]): List[A] = foldLeft(as, List())((z, h) => Cons(h, z))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // TODO 13

  def foldRightViaAppend[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a1, a2) => Cons(a1, a2))

  // TODO 15

}
