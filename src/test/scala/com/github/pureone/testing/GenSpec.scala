package com.github.pureone.testing

import com.github.pureone.state.RNG._
import com.github.pureone.state._
import org.scalatest.FreeSpec

class GenSpec extends FreeSpec {
  val rng = SimpleRNG(2)

//  "choose" - {
//    "" in {
//      val (n, _) = Gen.choose(1,5)
//        .sample
//        .run(SimpleRNG(6))
//      println(n)
//      assert(n <= 5 && n >= 1)
//    }
//  }
//
//  "boolean" - {
//    "" in {
//      println(Gen.boolean.sample.run(rng))
//    }
//  }

  "unit" - {
    "" in {
      println(Gen.unit(3).sample.run(SimpleRNG(2)))
    }
  }
//
//  "listOfN" - {
//    "" in {
//      val genList = Gen.liftOfN(5, Gen(State(RNG.nonNegativeInt)))
//      println(genList.sample.run(rng))
//    }
//  }
}
