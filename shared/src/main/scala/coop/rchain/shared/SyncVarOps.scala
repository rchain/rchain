package coop.rchain.shared

import scala.concurrent.SyncVar

object SyncVarOps {

  implicit class RichSyncVar[A](syncVar: SyncVar[A]) {
    def update(f: A => A): Unit = {
      val curr = syncVar.take()
      syncVar.put(f(curr))
    }
  }
}
