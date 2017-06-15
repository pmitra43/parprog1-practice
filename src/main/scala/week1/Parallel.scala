package week1


trait Task[A] {
  def join: A
}

object Parallel {
  def task[A](c: => A): Task[A] = { ??? }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val tB: Task[B] = task {taskB}
    val tA: A = taskA
    (tA, tB.join)
  }
}
