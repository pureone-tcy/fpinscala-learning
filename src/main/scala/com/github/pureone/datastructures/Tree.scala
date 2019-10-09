package com.github.pureone.datastructures

sealed trait Tree[+A] {

  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  def depth: Int = this match {
    case Branch(l, r) => 1 + (l.depth max r.depth)
    case Leaf(_)      => 1
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Branch(l, r) => Branch(l.map(f), r.map(f))
    case Leaf(v)      => Leaf(f(v))
  }

  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }

  def _size: Int = fold(_ => 1)(1 + _ + _)

  def _depth: Int = fold(_ => 1)((d1, d2) => 1 + (d1 max d2))

  def _map[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def maximum[A](t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(n)      => n
  }
  def _maximum[A](t: Tree[Int]): Int = t.fold(a => a)(_ max _)
}
