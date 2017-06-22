import week1.Parallel._

sealed abstract class Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]


def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l,r) => {
    val (lt, rt) = parallel(reduce[A](l, f), reduce[A](r, f))
    f(lt, rt)
  }
}

def tree=Node(Leaf(1), Node(Leaf(3), Leaf(8)))
def fMinus = (x: Int, y: Int) => x - y

reduce(tree, fMinus)

def map[A,B](t: Tree[A], f: A => B): Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Node(l,r) => Node(map(l, f), map(r, f))
}

def toList[A](t: Tree[A]): List[A] = t match {
  case Leaf(v) => List(v)
  case Node(l,r) => toList(l) ++ toList(r)
}

val t = Node(Leaf(1), Node(Leaf(2), Leaf(3)))

reduce(map(t, List(_)), _ ++ _)

val threshold = _

def reduceSeg[A](inp: Array[A], left: Int, right: Int, f:(A, A)=> A): A = {
  if(right - left < threshold) {
    var res = inp(left); var i = left + 1
    while(i<right){ res = f(res,inp(i)); i = i+1 }
    res
  }
  else {
    val mid = (right + left) / 2
    val (a1,a2) = parallel(reduceSeg(inp, left, mid, f),
                            reduceSeg(inp, mid, right, f))
    f(a1,a2)
  }
}

def reduce[A](inp: Array[A], f: (A, A) => A): A =
  reduceSeg(inp, 0, inp.length, f)

