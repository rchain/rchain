package org.lmdbjava
import org.lmdbjava.Library.LIB

object TxnOps {
  def manuallyAbortTxn[T](txn: Txn[T]) =
    LIB.mdb_txn_reset(txn.pointer())
}
