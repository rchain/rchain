package coop.rchain.casper.util.rholang

// package of Registry.scala:
// package coop.rchain.rholang.interpreter

import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GString}
import coop.rchain.models.{ETuple, Expr, GPrivate, Par}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.casper.protocol.DeployData
import coop.rchain.rholang.interpreter.{PrettyPrinter, Runtime}
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.rholang.implicits._

case class Derivation(
    sk: Hex,
    timestamp: Long,
    uname: Pretty,
    toSign: Hex,
    result: InsertSigned
) {
  override def toString() = s"""
    | sk = ${sk}
    | pk = ${result.pk}
    | timestamp = ${timestamp}
    | uname = ${uname}
    | nonce = ${result.nonce}
    | toSign = ${toSign}
    | sig = ${result.sig}
    |
    | ${result}
  """.stripMargin
}

case class InsertSigned(pk: Hex, nonce: Long, contract: String, sig: Hex) {
  override def toString() = s"""
    | rs!(
    |   \"${pk}\".hexToBytes(),
    |   (${nonce}, bundle+{${contract}}),
    |   \"${sig}\".hexToBytes(),
    |   Nil)
  """.stripMargin
}

case class Hex(bs: Array[Byte]) {
  override def toString() = Base16.encode(bs)
}
case class Pretty(p: Par) {
  override def toString() = PrettyPrinter().buildString(p)
}

object RegistrySigGen {
  import scala.math.pow
  val maxint: Long                                     = (1L << 62) + ((1L << 62) - 1)
  val byteArrayToByteString: Array[Byte] => ByteString = ba => ByteString.copyFrom(ba)
  val byteStringToExpr: ByteString => Expr             = bs => Expr(GByteArray(bs))
  val byteArrayToExpr: Array[Byte] => Expr             = byteArrayToByteString andThen byteStringToExpr

  def main(argv: Array[String]) = {
    // these could be command-line args...
    val term = insertSignedTerm(Ed25519.newKeyPair, System.currentTimeMillis, "CONTRACT")
    System.out.println(term)
  }

  def insertSignedTerm(key: (Array[Byte], Array[Byte]), timestamp: Long, contract: String) = {
    val (secKey, pubKey) = key
    val user             = byteArrayToByteString(pubKey)

    val seed        = DeployData().withUser(user).withTimestamp(timestamp)
    val rand        = Blake2b512Random(DeployData.toByteArray(seed))
    val id          = ByteString.copyFrom(rand.next())
    val uname: Par  = GPrivate(id)
    val nonce       = maxint
    val toSign: Par = ETuple(Seq(GInt(nonce), uname))
    val sig         = Ed25519.sign(toSign.toByteArray, secKey)

    Derivation(
      sk = Hex(secKey),
      timestamp = timestamp,
      uname = Pretty(uname),
      toSign = Hex(toSign.toByteArray),
      result = InsertSigned(
        Hex(pubKey),
        nonce,
        contract,
        Hex(sig)
      )
    )
  }
}
