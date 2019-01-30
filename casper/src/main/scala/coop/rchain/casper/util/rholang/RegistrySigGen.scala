package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GInt, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{Bundle, ETuple, Expr, GPrivate, Par}
import coop.rchain.rholang.interpreter.PrettyPrinter

/**
  * A signed insertion into the RChain registry.
  *
  * The `rho:registry:insertSigned:ed25519` system contract takes a
  * value to insert into the registry along with an ed25519 public key
  * and signature.  The value must be a pair (tuple of size 2) whose
  * first item is an integer nonce.
  *
  * Typically the second part of the value is the unforgeable name of a
  * contract, wrapped in a write-only bundle:
  *   `(value, bundle+{*c}).toByteArray`
  */
final case class InsertSigned(pk: Hex, value: (Long, Contract), sig: Hex) {
  def nonce    = value._1
  def contract = value._2

  override def toString() = s"""
    |new
    |  ${contract.varName}, rs(`rho:registry:insertSigned:ed25519`), uriOut
    |in {
    |  contract ${contract.varName}(...) = {
    |     ...
    |  } |
    |  rs!(
    |    \"${pk}\".hexToBytes(),
    |    (${nonce}, bundle+{*${contract.varName}}),
    |    \"${sig}\".hexToBytes(),
    |    *uriOut
    |  )
    |}
  """.stripMargin
}

final case class Hex(bs: Array[Byte]) {
  override def toString() = Base16.encode(bs)
}
final case class Contract(varName: String, p: Par)

/**
  * A signed insertion and its derivation from deployment parameters.
  *
  * To generate a signature, we need the corresponding secret key, of
  * course. See `RegistrSigGen.genIds` and `RegistrSigGen.deriveFrom`
  * for details.
  */
final case class Derivation(
    sk: Hex,
    timestamp: Long,
    uname: Par,
    toSign: Par,
    result: InsertSigned
) {
  override def toString() = s"""
    | /*
    |       given     1. sk = ${sk}
    |       given     2. timestamp = ${timestamp}
    |       lastNonce 3. nonce = ${result.nonce}
    | 1,    ed25519   4. pk = ${result.pk}
    | 4, 2, genIds    5. uname = ${pprint(uname)}
    | 3, 5, registry  6. value = ${pprint(toSign)}
    | 6,    protobuf  7. toSign = ${Hex(toSign.toByteArray)}
    | 7, 1, ed25519   8. sig = ${result.sig}
    | */
    |
    | ${result}
  """.stripMargin

  def pprint(p: Par) = PrettyPrinter().buildString(p)
}

object RegistrySigGen {
  val maxLong                                          = (1L << 62) + ((1L << 62) - 1)
  val byteArrayToByteString: Array[Byte] => ByteString = ba => ByteString.copyFrom(ba)

  /**
    * Usage:
    *
    * To print parameters for use with RChain rho:registry:insertSigned:ed25519
    * based on a current timestamp and a randomly chosen key, run this application.
    */
  def main(argv: Array[String]) = {
    // these could be command-line args...
    val info = deriveFrom(Ed25519.newKeyPair, System.currentTimeMillis, "CONTRACT")
    System.out.println(info)
  }

  def exampleMakeMint() = {
    val sk = Base16.decode("a300690f29ac6385917cb94bf534f9b4163792ef8636c5db44608a77fa0356c2");
    val pk = Ed25519.toPublic(sk)
    deriveFrom((sk, pk), 1539969637029L, "MakeMint")
  }

  /**
    * Seed deterministic unforgeable name sequence from deploy parameters.
    *
    * How can we know what unforgeable name will be chosen by `new c in {
    * ... }`?  Because all RChain validators must agree on how it's done,
    * a pseudorandom sequence of ids is seeded from the deploy parameters
    * `user` (public key) and `timestamp`.
    *
    */
  def genIds(user: ByteString, timestamp: Long) = {
    val seed = DeployData().withUser(user).withTimestamp(timestamp)
    Blake2b512Random(DeployData.toByteArray(seed))
  }

  /**
    * Derive a signature for use with rho:registry:insertSigned:ed25519
    * from an ed25519 key pair and a timestamp. The `varName` is a
    * hint/label; it doesn't affect the signature.
    */
  def deriveFrom(key: (Array[Byte], Array[Byte]), timestamp: Long, varName: String) = {
    val (secKey, pubKey) = key

    val user = byteArrayToByteString(pubKey)
    val id   = genIds(user, timestamp).next()

    // Now we know the unforgeable name
    // that will be allocated by
    //   new CONTRACT in { ... }
    // provided that's the first thing deployed.
    val uname: Par = GPrivate(ByteString.copyFrom(id))

    // Bundle the contract to prevent unauthorized reads.
    val access: Par = Bundle(uname, true, false)
    val contract    = Contract(varName, access)

    // Use the maxium nonce to prevent unauthorized updates.
    val lastNonce = maxLong

    // Now we can sign the value that goes in the registry.
    val toSign: Par = ETuple(Seq(GInt(lastNonce), access))
    val sig         = Ed25519.sign(toSign.toByteArray, secKey)

    Derivation(
      sk = Hex(secKey),
      timestamp = timestamp,
      uname = uname,
      toSign = toSign,
      result = InsertSigned(
        Hex(pubKey),
        (lastNonce, contract),
        Hex(sig)
      )
    )
  }
}
