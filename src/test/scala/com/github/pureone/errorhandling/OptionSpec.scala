package com.github.pureone.errorhandling

import org.scalatest.FreeSpec

class OptionSpec extends FreeSpec {
  "abs" - {
    "abs function." in {
      assert(Some(1d) == Option.abs(Some(-1d)))
    }
  }

  "sequence" - {
    "traverse." in {
      assert(
        Some(List(1, 2, 3)) == Option.sequence(List(Some(1), Some(2), Some(3)))
      )
    }
  }
}
