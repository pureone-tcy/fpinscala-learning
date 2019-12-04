package com.github.pureone.datastructures2

import org.scalatest.FreeSpec

class ListSpec extends FreeSpec {


  "setHead" - {
    "success" in {
      assert(
        List.setHead(List(1,2,3,4,5), 0) == List(0,2,3,4,5)
      )
    }
  }

  "drop" - {
    "success" in {
      assert(
        List.drop(List(1,2,3,4,5), 2) == List(3,4,5)
      )
    }
  }

  "dropWhile" - {
    "success" in {
      assert(List.dropWhile(List(1,2,3,4,5))(_ < 1) == List(1,2,3,4,5))
      assert(List.dropWhile(List(1,2,3,4,5))(_ < 5) == List(5))
      assert(List.dropWhile(Nil)(_ => true) == Nil)
    }
  }

  "length" - {
    "success" in {
      assert(List.length(List(1,1,1,1,1,1,1,1,1,1)) == 10)
    }
  }

  "sum2" - {
    "compare element of summary result." in {
      assert(
        List.sum2(List(1,2,3)) == 6
      )
    }
  }

  "sum3" - {
    "success" in {
      assert(
        List.sum3(List(1,2,3)) == 6
      )
    }
  }
}
