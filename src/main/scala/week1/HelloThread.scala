package week1

/**
  * Created by priyammitra on 6/14/17.
  */
class HelloThread extends Thread {
  override def run(): Unit = {
    println("Hello")
    println("Thread")
  }
}
