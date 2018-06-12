package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import java.nio.file.{Files, Path, StandardCopyOption}

import coop.rchain.catscontrib.Capture
import monix.execution.Scheduler

class Tuplespace(val name: String, val location: Path, val size: Long) {
  val dbLocation: Path = location.resolve(name)
  val runtime: Runtime = Runtime.create(dbLocation, size)(Capture.taskCapture)

  def addTerm(term: Par)(implicit scheduler: Scheduler): Unit =
    runtime.reducer.inj(term).unsafeRunSync

  def hash: Array[Byte] = {
    val bytes = ByteString.copyFromUtf8(StoragePrinter.prettyPrint(runtime.space.store))
    Blake2b256.hash(bytes.toByteArray)
  }

  def checkpoint: Checkpoint = {
    val hsh        = hash
    val hashString = Base16.encode(hsh)
    val dest       = location.resolve(hashString)

    dest.toFile.mkdir()
    Tuplespace.copyDB(dbLocation, dest)

    new Checkpoint(ByteString.copyFrom(hsh), location, size)
  }

  def storageRepr: String =
    StoragePrinter.prettyPrint(runtime.space.store)

  def delete(): Unit = {
    runtime.close()
    dbLocation.resolve("lock.mdb").toFile.delete()
    dbLocation.resolve("data.mdb").toFile.delete()
    dbLocation.toFile.delete()
  }
}

object Tuplespace {
  def copyDB(source: Path, dest: Path): Unit = {
    val srcLock = source.resolve("lock.mdb")
    val srcData = source.resolve("data.mdb")

    val destLock = dest.resolve("lock.mdb")
    val destData = dest.resolve("data.mdb")

    Files.copy(srcLock, destLock, StandardCopyOption.REPLACE_EXISTING)
    Files.copy(srcData, destData, StandardCopyOption.REPLACE_EXISTING)
  }

  def randomName: String = "ts-" + (scala.util.Random.nextInt(10000)).formatted("%04d")

  def apply(location: Path, size: Long): Tuplespace =
    new Tuplespace(randomName, location, size)
}
