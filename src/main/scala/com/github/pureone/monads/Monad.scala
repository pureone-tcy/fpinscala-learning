package com.github.pureone.monads

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B):F[B]
}


class Monad {
  val listF = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B) = fa map f
  }
}
