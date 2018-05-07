package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Sha256
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.storage.StoragePrinter

import java.nio.file.Path

import monix.execution.Scheduler

class Tuplespace(val location: Path, val size: Long) {
  val runtime = Runtime.create(location, size)

  def withTerm(term: Par, dest: Path)(implicit scheduler: Scheduler): Tuplespace = {
    InterpreterUtil.copyDB(location, dest)
    val result = new Tuplespace(dest, size)
    Tuplespace.updateInPlace(result, term)
  }

  def hash: ByteString = {
    val bytes = ByteString.copyFromUtf8(StoragePrinter.prettyPrint(runtime.store))
    val hsh   = Sha256.hash(bytes.toByteArray)
    ByteString.copyFrom(hsh)
  }
}

object Tuplespace {
  def updateInPlace(source: Tuplespace, term: Par)(implicit scheduler: Scheduler): Tuplespace = {
    source.runtime.reducer.inj(term).unsafeRunSync
    source
  }
}
