package com.github.pureone.state

import com.github.pureone.state.RNG.SimpleRNG
import org.scalatest.FreeSpec

class StateSpec extends FreeSpec {
  "map" - {
    "" in {
      val r1 = RNG.map(RNG.nonNegativeInt)(_.toString)(SimpleRNG(42))
      val r2 = RNG.nonNegativeInt(SimpleRNG(42))
      println(s"$r1 == $r2")
    }
  }
}
