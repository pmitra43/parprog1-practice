package week1

object Parallel {
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = { ??? }
}
