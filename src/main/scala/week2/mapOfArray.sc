import week1.Parallel
import week1.Parallel.parallel

def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int,
                     f: A => B, out: Array[B]) = {
  var i = left
  while(i < right){
    out(i)=f(inp(i))
    i = i + 1
  }
}
var threshold = _
def mapASegPar[A, B](inp: Array[A], left: Int, right: Int,
                     f: A => B, out: Array[B]): Unit = {
  if(right - left < threshold)
    mapASegSeq(inp, left, right, f, out)
  else{
    val mid = left + (right - left)/2
    parallel(mapASegPar(inp, left, mid, f, out),
      mapASegPar(inp, mid, right, f, out))
  }
}

val in = Array(1,2,3,4,5)
val out = Array(0,0,0,0,0)
val f = (x: Int) => x * x
mapASegSeq(in, 1, 3, f, out)
out
