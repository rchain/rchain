package coop.rchain.node.benchmark

import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.rholang.interpreter.util.RevAddress

final case class User(sk: PrivateKey, pk: PublicKey, addr: String) {
  override def equals(obj: Any): Boolean = obj match {
    case User(_, _, a) => a == addr
    case _             => false
  }
  override def hashCode(): Int = addr.hashCode()
}

object User {
  def random: Iterator[User] =
    Iterator.continually(Secp256k1.newKeyPair).map {
      case (sk, pk) => User(sk, pk, RevAddress.fromPublicKey(pk).get.address.toBase58)
    }
}
