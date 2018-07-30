package org.lmdbjava
import org.lmdbjava.Library.LIB

object TxnOps {

  /**
    * a copy of rchain/rspace/src/main/scala/org/lmdbjava/TxnOps.scala
    */
  def manuallyAbortTxn[T](txn: Txn[T]) =
    LIB.mdb_txn_reset(txn.pointer())
}
