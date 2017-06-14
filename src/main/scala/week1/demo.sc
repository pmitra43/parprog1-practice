import week1.HelloThread

val t = new HelloThread
val s = new HelloThread
t.start()
s.start()
t.join()
s.join()
