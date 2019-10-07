package com.github.pureone.datastructures

import org.scalatest.FreeSpec

class TreeSpec extends FreeSpec {

  val tree: Tree[Int] =
    Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(Leaf(3),
        Branch(Leaf(4), Leaf(5))))

  "size" - {
    "Tree size." in {
      assert(9 == tree.size)
    }
  }

  "maximum" - {
    "max num." in {
      assert(5 == Tree.maximum(tree))
    }
  }

  "depth" - {
    "depth num." in {
      assert(4 == tree.depth)
    }
  }
}
