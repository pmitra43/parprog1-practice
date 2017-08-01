import java.util.concurrent._

import scala.collection.{GenSeq, GenSet}

def sum(xs: Array[Int]): Int =
  xs.par.fold(0)(_ + _)

val xs = Array(1,2,3,4)

def max(xs: Array[Int]): Int =
  xs.par.fold(Int.MinValue)(math.max)

max(xs)

Array("paper", "rock", "paper", "scissors")

def isVowel(c: Char): Boolean = c == 'A' || c == 'E' ||
c == 'I' || c == 'O' || c == 'U'

Array('E', 'P', 'F', 'L').par
.aggregate(0)((count, c) => if(isVowel(c)) count + 1 else count, _ + _)

def largestPallindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)(
    (largest, n) =>
      if(n > largest && n.toString == n.toString.reverse) n else largest,
    math.max)
}

val array = (0 to 100000).toArray
val parArray = (0 to 100000).toParArray
largestPallindrome(array)
largestPallindrome(parArray)

def intersection(a: GenSet[Int], b: GenSet[Int]) = {
  val result = new ConcurrentSkipListSet[Int]()
  for(x <- a) if(b contains x) result += x
  result
}