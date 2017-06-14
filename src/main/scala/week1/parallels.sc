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

def pNormFourPart(a: Array[Int], p: Double): Int = {
  val (m1, m2, m3) = (a.length / 4, a.length/2, 3*a.length/4)
  val ((sum1, sum2),(sum3, sum4)) = parallel(parallel(sumSegments(a, p, 0, m1), sumSegments(a, p, m1, m2)),
  parallel(sumSegments(a, p, m2, m3), sumSegments(a, p, m3, a.length)))
  power(sum1 + sum2, 1/p)
}

def pNormRec(a: Array[Int], p: Double): Int =
power(segmentRec(a, p, 0, a.length), 1/p)

def threshold: Int = ???
def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  if(t - s < threshold)
    sumSegments(a,p,s,t)
  else {
    val m = s + (t-s)/2
    val (sum1, sum2) = parallel(segmentRec(a,p,0,m),
                                segmentRec(a,p,m,a.length))
    sum1 + sum2
  }
}