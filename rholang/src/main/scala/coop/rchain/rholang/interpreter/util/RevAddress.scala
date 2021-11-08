package coop.rchain.rholang.interpreter.util
import coop.rchain.crypto.PublicKey
import coop.rchain.models.{GPrivate, Validator}
import coop.rchain.shared.Base16

final case class RevAddress(address: Address) {

  def toBase58: String = address.toBase58
}

object RevAddress {

  private val coinId  = "000000"
  private val version = "00"
  private val prefix  = Base16.unsafeDecode(coinId + version)

  private val tools = new AddressTools(prefix, keyLength = Validator.Length, checksumLength = 4)

  def fromDeployerId(deployerId: Array[Byte]): Option[RevAddress] =
    fromPublicKey(PublicKey(deployerId))

  def fromPublicKey(pk: PublicKey): Option[RevAddress] =
    tools.fromPublicKey(pk).map(RevAddress(_))

  def fromEthAddress(ethAddress: String): Option[RevAddress] =
    tools.fromEthAddress(ethAddress).map(RevAddress(_))

  def fromUnforgeable(gprivate: GPrivate): RevAddress =
    RevAddress(tools.fromUnforgeable(gprivate))

  def parse(address: String): Either[String, RevAddress] =
    tools.parse(address).map(RevAddress(_))

  def isValid(address: String): Boolean = parse(address).isRight
}
