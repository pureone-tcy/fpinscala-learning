package com.github.pureone.monoids

import org.scalatest.FreeSpec

class MonoidSpec extends FreeSpec {

  "op" - {
    "success" in {
      import Monoid._

      val words = List("Hic", "Est", "Index")
      println(words.foldRight(stringMonoid.zero)(stringMonoid.op))
      println(words.foldLeft(stringMonoid.zero)(stringMonoid.op))

    }
  }
}
