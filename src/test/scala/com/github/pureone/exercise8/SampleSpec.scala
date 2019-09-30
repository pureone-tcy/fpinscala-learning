package com.github.pureone.exercise8

import org.scalatest._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop

class SampleSpec extends FlatSpec {

  val intList = Gen.listOf(Gen.choose(0, 100))
  val prop: org.scalacheck.Prop =
    forAll(intList)(ns => ns.reverse.reverse == ns) &&
      forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
  val failingProp = forAll(intList)(ns => ns.reverse == ns)

  prop.check
  failingProp.check

  val in: Gen[Int] => Gen[List[Int]] = Gen.listOf(_)

}
