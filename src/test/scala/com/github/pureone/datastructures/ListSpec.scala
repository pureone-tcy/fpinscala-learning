package com.github.pureone.datastructures

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  import List._

  "foldRight" should "sum." in {
    assertResult(6) {
      foldRight(List(1,2,3), 0)(_+_)
    }
  }

  "foldLeft" should "sum." in {
    assertResult(6) {
      foldLeft(List(1,2,3), 0)(_+_)
    }
  }

  "foldRightViaFoldLeft" should "sum." in {
    assertResult(6) {
      foldRightViaFoldLeft(List(1,2,3), 0)(_ + _)
    }
  }

  "foldLeftViaFoldRight" should "sum." in {
    assertResult(6) {
      foldLeftViaFoldRight(List(1,2,3), 0)(_ + _)
    }
  }

  "append" should "join." in {
    assertResult(List(1,2,3,4,5,6)) {
      append(List(1,2,3), List(4,5,6))
    }
  }

  "concat" should "append List." in {
    assertResult(List(1,2,3,4)) {
      concat(List(List(1), Nil, List(2), List(3,4)))
    }
    assertResult(List(1,2,3,4)) {
      _concat(List(List(1), List(2,3), List(), List(4)))
    }
  }

  "map" should "multiplication." in {
    assertResult(List(2,4,6)) {
      map(List(1,2,3))(_ * 2)
    }
  }
  "flatMap" should "add element twice." in {
     assertResult(List(1,1,2,2,3,3)) {
     flatMap(List(1,2,3))(a => List(a, a))
   }
  }

  "hasSubsequence" should "sub sequence match." in {
    assertResult(true) {
      hasSubsequence(List(1,2,3), List(2,3))
    }
    assertResult(false) {
      hasSubsequence(List(1,2,3), List(1,3))
    }
  }

}
