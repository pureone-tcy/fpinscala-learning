package com.github.pureone.laziness

import org.scalatest.FreeSpec

class StreamSpec extends FreeSpec {
  "toList" - {
    "Stream to List." in {
      assert(List(1, 2, 3) == Stream(1, 2, 3).toList)
    }
  }
}
