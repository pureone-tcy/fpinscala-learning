//package com.github.pureone.testing
//
//import com.github.pureone.state.RNG._
//import com.github.pureone.state._
//import org.scalatest.FreeSpec
//
//class GenSpec extends FreeSpec {
//  val rng = Simple(2)
//
//  "choose" - {
//    "" in {
//      val (n, _) = Gen.choose(1,5)
//        .sample
//        .run(Simple(6))
//      assert(n <= 5 && n >= 1)
//    }
//  }
//
//  "boolean" - {
//    "" in {
//      assert(Gen.boolean.sample.run(Simple(1))._1)
//    }
//  }
//
//  "unit" - {
//    "" in {
//      assert(3 == Gen.unit(3).sample.run(Simple(2))._1)
//    }
//  }
//
//  "listOfN" - {
//    "" in {
//      val genList = Gen.listOfN(5, Gen(State(RNG.nonNegativeInt)))
//      assert(5 == genList.sample.run(rng)._1.size)
//    }
//  }
//}
