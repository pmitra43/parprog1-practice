class HelloThread extends Thread {
  override def run(): Unit = {
    println("Hello")
    println("Thread")
  }
}

val t = new HelloThread
val s = new HelloThread
t.start()
s.start()
t.join()
s.join()

val x = new AnyRef {}
private var uidCount = 0L

def getUniqueId: Long = x.synchronized {
  uidCount = uidCount + 1
  uidCount
}

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for(_ <- 0 until 10) yield getUniqueId
      println(uids)
    }
  }
  t.start()
  t
}

class Account(private var amount: Int = 0) {
  def transfer(target: Account, n: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount +=n
      }
    }
  }
}

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (i <- 0 until n) {
        a.transfer(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(500000)
val a2 = new Account(500000)

val t1 = startThread(a1, a2, 150000)
val s1 = startThread(a2, a1, 150000)
t1.join()
s1.join()
