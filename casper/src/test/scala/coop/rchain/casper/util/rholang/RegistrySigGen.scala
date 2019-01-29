package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{Bundle, ETuple, Expr, GPrivate, Par}
import coop.rchain.rholang.interpreter.{PrettyPrinter, Runtime}

case class InsertSigned(pk: Hex, value: (Long, Contract), sig: Hex) {
  def nonce    = value._1
  def contract = value._2

  override def toString() = s"""
    | rs!(
    |   \"${pk}\".hexToBytes(),
    |   (${nonce}, bundle+{*${contract.varName}}),
    |   \"${sig}\".hexToBytes(),
    |   *uriOut)
  """.stripMargin
}

case class Hex(bs: Array[Byte]) {
  override def toString() = Base16.encode(bs)
}
case class Contract(varName: String, p: Par)

case class Derivation(
    sk: Hex,
    timestamp: Long,
    uname: Par,
    toSign: Hex,
    result: InsertSigned
) {
  override def toString() = s"""
    | sk = ${sk}
    | pk = ${result.pk}
    | timestamp = ${timestamp}
    | uname = ${PrettyPrinter().buildString(uname)}
    | nonce = ${result.nonce}
    | toSign = ${toSign}
    | sig = ${result.sig}
    |
    | ${result}
  """.stripMargin
}

object RegistrySigGen {
  val maxLong                                          = (1L << 62) + ((1L << 62) - 1)
  val byteArrayToByteString: Array[Byte] => ByteString = ba => ByteString.copyFrom(ba)

  def main(argv: Array[String]) = {
    // these could be command-line args...
    val info = deriveFrom(Ed25519.newKeyPair, System.currentTimeMillis, "CONTRACT")
    System.out.println(info)
  }

  def deriveFrom(key: (Array[Byte], Array[Byte]), timestamp: Long, varName: String) = {
    val (secKey, pubKey) = key

    val user = byteArrayToByteString(pubKey)
    val id   = genIds(user, timestamp).next()

    // Now we can determine the unforgeable name
    // that will be allocated by
    //   new CONTRACT in { ... }
    // provided that's the first thing deployed.
    val uname: Par = GPrivate(ByteString.copyFrom(id))

    // Nobody else can receive from our contract.
    val access: Par = Bundle(uname, true, false)
    val contract    = Contract(varName, access)

    // prevent anyone from replacing this registry entry by using the maximum nonce value.
    val lastNonce = maxLong

    // Now we have the value that goes in the registry and we can sign it.
    val toSign: Par = ETuple(Seq(GInt(lastNonce), access))
    val sig         = Ed25519.sign(toSign.toByteArray, secKey)

    Derivation(
      sk = Hex(secKey),
      timestamp = timestamp,
      uname = uname,
      toSign = Hex(toSign.toByteArray),
      result = InsertSigned(
        Hex(pubKey),
        (lastNonce, contract),
        Hex(sig)
      )
    )
  }

  /** Seed source of deterministic unforgeable names with
    * deploy parameters: user (public key) and timestamp.
    */
  def genIds(user: ByteString, timestamp: Long) = {
    val seed = DeployData().withUser(user).withTimestamp(timestamp)
    Blake2b512Random(DeployData.toByteArray(seed))
  }

}
