package coop.rchain.node.configuration.commandline
import coop.rchain.crypto.PublicKey
import org.rogach.scallop.{ArgType, ValueConverter}
import cats.implicits._
import coop.rchain.crypto.signatures.Ed25519

object Ed25519PubKeyConverter extends ValueConverter[PublicKey] {

  private def toEd25519PublicKey(bytes: Array[Byte]): Either[String, PublicKey] =
    if (bytes.length == Ed25519.publicKeyLength)
      Right(PublicKey(bytes))
    else
      Left("Invalid key length")

  override def parse(s: List[(String, List[String])]): Either[String, Option[PublicKey]] =
    Base16Converter.parse(s) >>= (_.traverse(toEd25519PublicKey))

  override val argType: ArgType.V = ArgType.SINGLE
}
