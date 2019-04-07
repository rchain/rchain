package coop.rchain.crypto.util

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16

object Sorting {

  implicit val byteArrayOrdering: Ordering[Array[Byte]] =
    Ordering.by(Base16.encode)

  implicit val publicKeyOrdering: Ordering[PublicKey] =
    (x: PublicKey, y: PublicKey) => byteArrayOrdering.compare(x.bytes, y.bytes)
}
