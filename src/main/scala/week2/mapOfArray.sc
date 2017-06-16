def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int,
                     f: A => B, out: Array[B]) = {
  var i = left
  while(i < right){
    out(i)=f(inp(i))
    i = i + 1
  }
}

val in = Array(1,2,3,4,5)
val out = Array(0,0,0,0,0)
val f = (x: Int) => x * x
mapASegSeq(in, 0, 5, f, out)
out
