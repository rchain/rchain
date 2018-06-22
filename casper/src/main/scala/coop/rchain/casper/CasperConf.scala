package coop.rchain.casper

import coop.rchain.crypto.codec.Base16

import java.nio.file.Path

case class CasperConf(
    pkBase16: Option[String],
    skBase16: String,
    sigAlgorithm: String,
    bondsFile: Option[String],
    numValidators: Int,
    validatorsPath: Path,
    storageLocation: Path,
    storageSize: Long
) {
  val pk: Option[Array[Byte]] = pkBase16.map(Base16.decode)
  val sk: Array[Byte]         = Base16.decode(skBase16)
}
