package example.exercise3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](leaf: Tree[A], right: Tree[A]) extends Tree[A]

object Exercise3_2 {

  // Exercise 24
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => println("TODO"); hasSubsequence(t, sub)
  }

  // Exercise 25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 26
  def maxinum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maxinum(l) max maxinum(r)
  }

  // Exercise 27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 29


  def main(args: Array[String]): Unit = {
    val obj = Exercise3_2

    val listA = List(1,2,3,4,5)
    val listB = List(3,4)

    val res = obj.hasSubsequence(listA, listB)

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val resTree = obj.maxinum(tree)
    println(resTree)
  }
}
