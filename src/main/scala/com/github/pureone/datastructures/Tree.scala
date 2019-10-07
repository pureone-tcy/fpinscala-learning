package com.github.pureone.datastructures

sealed trait Tree[+A] {
  private val t = Tree
  def size: Int = t.size(this)
  def _size: Int = t._size(this)
  def depth: Int = t.depth(this)
  def _depth: Int = t.depth(this)
  def map[B](f: A => B): Tree[B] = t.map(this)(f)
  def _map[B](f: A => B): Tree[B] = t._map(this)(f)
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum[A](t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(n)      => n
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(_)      => 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v)      => Leaf(f(v))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def _size[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def _maximum[A](t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def _depth[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ max _)

  def _map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
