package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Blake2b512Random}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GInt, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{Bundle, ETuple, Expr, GPrivate, Par}
import coop.rchain.rholang.interpreter.{PrettyPrinter, Registry}

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
    result: InsertSigned,
    uri: String
) {
  override def toString() = s"""
    | /*
    | The table below describes the required computations and their dependencies
    |
    | No. | Dependency | Computation method | Result
    | ----+------------+--------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------
    | 1.  |            | given              | sk = ${sk}
    | 2.  |            | given              | timestamp = ${timestamp}
    | 3.  |            | lastNonce          | nonce = ${result.nonce}
    | 4.  | 1,         | ed25519            | pk = ${result.pk}
    | 5.  | 4, 2,      | genIds             | uname = ${pprint(uname)}
    | 6.  | 3, 5,      | registry           | value = ${pprint(toSign)}
    | 7.  | 6,         | protobuf           | toSign = ${Hex(toSign.toByteArray)}
    | 8.  | 7, 1,      | ed25519            | sig = ${result.sig}
    | 9.  | 4,         | registry           | uri = ${uri}
    | ----+------------+--------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------
    | */
    |
    | ${result}
  """.stripMargin

  def pprint(p: Par) = PrettyPrinter().buildString(p)
}

final case class Args(
    keyPair: (Array[Byte], Array[Byte]),
    timestamp: Long,
    unforgeableName: Par,
    contractName: String
)

object Args {
  def apply(
      contractName: String = "CONTRACT",
      timestamp: Long = System.currentTimeMillis,
      skOption: Option[Array[Byte]] = None,
      unforgeableNameStr: Option[String] = None
  ): Args = {
    val keyPair =
      skOption
        .map(sk => (sk, Ed25519.toPublic(sk)))
        .getOrElse(Ed25519.newKeyPair)

    val id =
      unforgeableNameStr
        .map(Base16.decode(_))
        .getOrElse(RegistrySigGen.generateUnforgeableNameId(keyPair._1, timestamp))

    // Now we can determine the unforgeable name
    // that will be allocated by
    //   new CONTRACT in { ... }
    // provided that's the first thing deployed.
    val unforgeableName = GPrivate(ByteString.copyFrom(id))

    Args(keyPair, timestamp, unforgeableName, contractName)
  }

  def parse(argv: Array[String]) =
    argv match {
      case Array(contractName, timestampStr, skBase16, unforgeableName) =>
        Args(
          contractName,
          timestampStr.toLong,
          Some(Base16.decode(skBase16)),
          Some(unforgeableName)
        )
      case Array(contractName, timestampStr, skBase16) =>
        Args(
          contractName,
          timestampStr.toLong,
          Some(Base16.decode(skBase16))
        )
      case Array(contractName, timestampStr) => Args(contractName, timestampStr.toLong)
      case Array(contractName)               => Args(contractName)
    }
}

object RegistrySigGen {
  val maxLong = (1L << 62) + ((1L << 62) - 1)

  /**
    * Usage:
    *
    * sbt runMain coop.rchain.casper.util.rholang.RegistrySigGen contractName [timestamp] [privateKey] [unforgeableName]
    *
    * Example:
    * runMain coop.rchain.casper.util.rholang.RegistrySigGen MyContract 1539808849271 adf1e344dd34979255aeaaf5703a586fba52246d9940ef58572572db339316d3 bc4bad752f043b9e488ddfc1c69370a89700f4852f318a79d0b50d88f44e1210
    *
    * For a new contract one should provide only the contract name.
    *
    * For verifying an existing deploy one can provide all the parameters.
    */
  def main(argv: Array[String]) = {
    // these could be command-line args...
    val info = RegistrySigGen.deriveFrom(Args.parse(argv))
    System.out.println(info)
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
  def generateUnforgeableNameId(deployer: Array[Byte], timestamp: Long) = {
    val seed =
      DeployData()
        .withDeployer(ByteString.copyFrom(deployer))
        .withTimestamp(timestamp)

    val rnd = Blake2b512Random(DeployData.toByteArray(seed))

    rnd.next()
  }

  /**
    * Derive a signature for use with rho:registry:insertSigned:ed25519
    * from an ed25519 key pair and a timestamp. The `contractName` is a
    * hint/label; it doesn't affect the signature.
    */
  def deriveFrom(args: Args) = {
    val (secKey, pubKey) = args.keyPair

    // Bundle the contract to prevent unauthorized reads.
    val access: Par = Bundle(args.unforgeableName, true, false)
    val contract    = Contract(args.contractName, access)

    // Use the maxium nonce to prevent unauthorized updates.
    val lastNonce = maxLong

    // Now we can sign the value that goes in the registry.
    val toSign: Par = ETuple(Seq(GInt(lastNonce), access))
    val sig         = Ed25519.sign(toSign.toByteArray, secKey)

    val keyHash = Blake2b256.hash(pubKey)
    val uri     = Registry.buildURI(keyHash)

    Derivation(
      sk = Hex(secKey),
      timestamp = args.timestamp,
      uname = args.unforgeableName,
      toSign = toSign,
      result = InsertSigned(
        Hex(pubKey),
        (lastNonce, contract),
        Hex(sig)
      ),
      uri
    )
  }
}
