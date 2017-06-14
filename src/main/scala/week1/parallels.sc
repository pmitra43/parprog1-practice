import scala.math.{abs, exp, log}

def sumSegments(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s
  var sum: Int = 0
  while (i < t) {
    sum += power(a(i), p)
    i = i + 1
  }
  sum
}

def power(x: Int, p: Double) = exp(p * log(abs(x))).toInt

def pNorm(a: Array[Int], p: Double): Int =
  power(sumSegments(a, p, 0, a.length), 1/p)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = { ??? }

def pNormTwoPart(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = parallel(sumSegments(a, p, 0, m), sumSegments(a, p, m, a.length))
  power(sum1 + sum2, 1/p)
}