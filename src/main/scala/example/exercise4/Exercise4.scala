package example.exercise4

sealed trait Option[+A] { self =>

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = self match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = self match {
    case Some(x) => f(x)
    case None => None
  }
  def flatMap2[B](f: A => Option[B]): Option[B] = {
    map (f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = self match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = self match {
    case None => ob
    case _ => self

  }
  def orElse2[B >: A](ob: => Option[B]): Option[B] = {
    map (Some(_)) getOrElse ob
  }

  def filter1(f: A => Boolean): Option[A] = self match {
    case Some(x) if f(x) => self
    case None => None
  }
  def filter2(f: A => Boolean): Option[A] =
    flatMap (a => if(f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
object None extends Option[Nothing]


object Exercise4 {
  def main(args: Array[String]): Unit = {

  }
}

class Upper
class Middle extends Upper
class Lower extends Middle

class Container[-A] {
  def foo[E <: A](x: E): E = x
}

