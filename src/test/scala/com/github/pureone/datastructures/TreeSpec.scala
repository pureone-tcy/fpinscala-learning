package com.github.pureone.datastructures

import org.scalatest.FreeSpec

class TreeSpec extends FreeSpec {

  val tree: Tree[Int] =
    Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

  "size" - {
    "Tree size." in {
      assert(9 == tree.size)
      assert(9 == tree._size)
    }
  }

  "maximum" - {
    "max num." in {
      assert(5 == Tree.maximum(tree))
      assert(5 == Tree._maximum(tree))
    }
  }

  "depth" - {
    "depth num." in {
      assert(4 == tree.depth)
      assert(4 == tree._depth)
    }
  }

  "map" - {
    "twice." in {
      val expect: Tree[Int] =
        Branch(Branch(Leaf(2), Leaf(4)),
               Branch(Leaf(6), Branch(Leaf(8), Leaf(10))))

      assert(expect == tree.map(_ * 2))
      assert(expect == tree._map(_ * 2))
    }
  }
}
