package coop.rchain.shared

import java.util.concurrent.LinkedBlockingQueue

object SyncVarOps {

  implicit class RichSyncVar[A](syncVar: LinkedBlockingQueue[A]) {
    def update(f: A => A): Unit = {
      val curr = syncVar.take()
      syncVar.put(f(curr))
    }
  }

  def create[A](a: A): LinkedBlockingQueue[A] = synchronized {
    val r = new LinkedBlockingQueue[A](1)
    r.put(a)
    r
  }

}
