package coop.rchain.rholang.interpreter.util
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16

final case class RevAddress(val keyHash: Array[Byte])

object RevAddress {
  private val coinId  = "000000"
  private val version = "00"
  private val prefix  = Base16.unsafeDecode(coinId + version)

  private val tools = new AddressTools(prefix, keyLength = 32, checksumLength = 4)

  def fromPublicKey(pk: PublicKey): Option[String] = tools.fromPublicKey(pk)

  def parse(address: String) = tools.parse(address).map(a => RevAddress(a.keyHash))

  def isValid(address: String) = parse(address).isRight
}
