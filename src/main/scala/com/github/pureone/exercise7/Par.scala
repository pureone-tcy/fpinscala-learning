package com.github.pureone.exercise7

object Par {

  def unit[A](a: A): Par[A] = Par(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    Par(f(a.a, b.a))

}

case class Par[A](a: A)
