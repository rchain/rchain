package coop.rchain.rholang.interpreter.utils

import java.nio.file.Files

import coop.rchain.models.{BindPattern, Channel, TaggedContinuation}
import coop.rchain.rspace.{IStore, LMDBStore}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rholang.interpreter.utils.PersistentStoreTester
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import coop.rchain.rspace.{IStore, LMDBStore, Serialize}

trait PersistentStoreTester {
  def withTestStore[R](f: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation] => R): R = {
    val dbDir = Files.createTempDirectory("rchain-storage-test-")
    val store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation] = LMDBStore.create[Channel, BindPattern, Seq[Channel], TaggedContinuation](dbDir, 1024 * 1024 * 1024)
    try {
      f(store)
    } finally {
      store.close()
    }
  }
}