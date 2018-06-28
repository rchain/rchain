package coop.rchain.casper

import coop.rchain.crypto.codec.Base16

import java.nio.file.Path

case class CasperConf(
    publicKeyBase16: Option[String],
    privateKeyBase16: Option[String],
    sigAlgorithm: String,
    bondsFile: Option[String],
    numValidators: Int,
    validatorsPath: Path
) {
  val publicKey: Option[Array[Byte]]  = publicKeyBase16.map(Base16.decode)
  val privateKey: Option[Array[Byte]] = privateKeyBase16.map(Base16.decode)
}
