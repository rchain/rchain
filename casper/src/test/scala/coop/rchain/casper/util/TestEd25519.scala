package coop.rchain.casper.util

import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.crypto.{PrivateKey, PublicKey}

object TestEd25519 {

  val keypair1: (PrivateKey, PublicKey) = Ed25519.newKeyPair
  val keypair2: (PrivateKey, PublicKey) = keypair1
  val keypair3: (PrivateKey, PublicKey) = keypair1
  val keypair4: (PrivateKey, PublicKey) = keypair1

  private val keypairs: Vector[(PrivateKey, PublicKey)] =
    Vector(keypair1, keypair2, keypair3, keypair4)

  def keypairs(n: Int): Vector[(PrivateKey, PublicKey)] = {
    require(
      n <= keypairs.size,
      s"There are only ${keypairs.size} predefined keypairs, but $n were requested. Come here and add some."
    )
    keypairs.take(n)
  }

}
