package com.github.pureone.exercise8

trait Prop {

  // Ex3
//  def check: Boolean
//  def &&(p: Prop): Prop = new Prop {
//    def check: Boolean = Prop.this.check && p.check
//  }

  import Prop._
  def check: Either[FailedCase, SuccessCount]

  // Ex4
  import com.github.pureone.exercise6.RNG
  import com.github.pureone.exercise6.State
  case class Gen[A](sample: State[RNG, A])
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}