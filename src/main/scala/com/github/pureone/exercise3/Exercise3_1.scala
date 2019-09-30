package com.github.pureone.exercise3

sealed trait List[+A] {

  // Exercise 2
  def tail: List[A]

  // Exercise 4
  def drop(n: Int): List[A] = {
    def go(l: List[A], n: Int): List[A] = l match {
      case Cons(_, xs) if n > 0 => go(xs, n - 1)
      case Cons(_, xs)          => xs
      case _                    => Nil // TODO sys.error("OutOfBoundsException")
    }
    go(this, n)
  }

  // Exercise 5
  def dropWhile(f: A => Boolean): List[A] = {
    def go(l: List[A]): List[A] = l match {
      case Cons(x, xs) if f(x) => xs
      case Cons(_, xs)         => go(xs)
      case _                   => Nil
    }
    go(this)
  }
}
case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = Nil // TODO action
}
case class Cons[+A](head: A, tl: List[A]) extends List[A] {
  override def tail: List[A] = tl
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  // Exercise 3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Cons(_, xs) => Cons(a, xs)
    case Nil         => Nil
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil         => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
    case Nil          => Nil
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, z) => z + 1)

  // Exercise 10
  def foldLeft[A, B](as: List[A], b: B)(f: (B, A) => B): B = as match {
    case Nil         => b
    case Cons(a, xs) => foldLeft(xs, f(b, a))(f)
  }

  // Exercise 11
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((b, _) => b + 1)

  // Exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((b, a) => Cons(a, b))

  // Exercise 13
  def foldRightViaFoldLeft[A, B](l: List[A], b: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), b)((b, a) => f(a, b))

  def foldRightViaFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // Exercise 14
  def append2[A](al: List[A], bl: List[A]): List[A] =
    foldRight(al, bl)(Cons(_, _))

  // Exercise 15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  // Exercise 16
  def add(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // Exercise 17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  // Exercise 18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // Exercise 19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  // Exercise 20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  // Exercise 22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil)                     => Nil
    case (Nil, _)                     => Nil
    case (Cons(a1, a2), Cons(b1, b2)) => Cons(a1 + b1, addPairwise(a2, b2))
  }

  // Exercise 23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (_, Nil)                     => Nil
      case (Nil, _)                     => Nil
      case (Cons(a1, a2), Cons(b1, b2)) => Cons(f(a1, b1), zipWith(a2, b2)(f))
    }
}

object Exercise3_1 {
  def main(args: Array[String]): Unit = {

    println(
      List.filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))((x: Int) => x % 2 != 0)
    )

    println(List.flatMap(List(1, 2, 3, 4, 5))(i => List(i, i)))

    println(List.filterViaFlatMap(List(1, 2, 3, 4, 5))((x: Int) => x % 2 != 0))

    println(
      List.zipWith(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5))((a, b) => a + b)
    )

    Option(2)

  }
}
