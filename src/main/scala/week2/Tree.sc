sealed abstract class Tree[A] { val size: Int }

case class Leaf[A](a: Array[A]) extends Tree[A]{
  override val size = a.size
}

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
  override val size = l.size + r.size
}