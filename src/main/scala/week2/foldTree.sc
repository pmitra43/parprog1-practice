import week1.Parallel._

sealed abstract class Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]


def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l,r) => f(reduce[A](l, f), reduce[A](r, f))
}

def tree=Node(Leaf(1), Node(Leaf(3), Leaf(8)))
def fMinus = (x: Int, y: Int) => x - y

reduce(tree, fMinus)