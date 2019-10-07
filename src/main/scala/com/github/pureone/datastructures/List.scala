package com.github.pureone.datastructures

sealed trait List[+A] {
  import List._

  def reverse: List[A] = foldLeft(this, Nil: List[A])((xs, a) => Cons(a, xs))
}
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    val func: B => B = foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))
    func(z)
  }

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def append[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)(Cons(_,_))

  def concat[A](a: List[List[A]]): List[A] = a match {
    case Nil => Nil
    case Cons(x, xs) => append(x, concat(xs))
  }

  def _concat[A](a: List[List[A]]): List[A] = foldRight(a, Nil: List[A])(append)

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def _map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil: List[B])((b, a) => Cons(f(a), b)).reverse

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((xs, a) => Cons(a, xs))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }



}
