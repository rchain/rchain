package coop.rchain.storage

import java.nio.ByteBuffer

import org.lmdbjava.Txn

object implicits {

  implicit object txnTransaction extends Transaction[Txn[ByteBuffer]] {

    def commit(t: Txn[ByteBuffer]): Unit = t.commit()

    def abort(t: Txn[ByteBuffer]): Unit = t.abort()

    def close(t: Txn[ByteBuffer]): Unit = t.close()
  }
}
