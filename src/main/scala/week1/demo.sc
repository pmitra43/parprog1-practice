import week1.HelloThread

val t = new HelloThread
val s = new HelloThread
t.start()
s.start()
t.join()
s.join()

private val x = new AnyRef {}
private var uidCount = 0L

def getUniqueId: Long = x.synchronized {
  uidCount = uidCount + 1
  uidCount
}

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for(i<- 0 until 10) yield getUniqueId
      println(uids)
    }
  }
  t.start()
  t
}

startThread(); startThread()