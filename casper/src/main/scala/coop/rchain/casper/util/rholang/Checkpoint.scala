package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.crypto.codec.Base16

import java.nio.file.Path

class Checkpoint(val hash: ByteString, val location: Path, val size: Long) {
  val dbLocation: Path = location.resolve(Base16.encode(hash.toByteArray))

  def toTuplespace(name: String): Tuplespace = {
    val tsLocation = location.resolve(name)

    tsLocation.toFile.mkdir()
    Tuplespace.copyDB(dbLocation, tsLocation)

    new Tuplespace(name, location, size)
  }

  def toTuplespace: Tuplespace = {
    val name       = Tuplespace.randomName
    val tsLocation = location.resolve(name)

    tsLocation.toFile.mkdir()
    Tuplespace.copyDB(dbLocation, tsLocation)

    new Tuplespace(name, location, size)
  }
}
