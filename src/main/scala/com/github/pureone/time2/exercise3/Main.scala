package com.github.pureone.time2.exercise3

object Main {
  def main(args: Array[String]): Unit = {

    val a1 = List(1, 2, 3, 4, 5)
    val a2 = List(6, 7, 8, 9, 10)

    val res = List.foldRightViaAppend(a1, a2)

    println(res)

  }
}
