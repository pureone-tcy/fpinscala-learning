package com.github.pureone.errorhandling

trait Option[+A] {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _       => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _       => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def variance(xs: Seq[Double]): Option[Double] = ???

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
