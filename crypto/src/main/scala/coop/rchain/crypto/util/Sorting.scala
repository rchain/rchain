package coop.rchain.crypto.util

import coop.rchain.crypto.PublicKey
import coop.rchain.shared.Base16

object Sorting {

  implicit val byteArrayOrdering = Ordering.by((_: Array[Byte]).toIterable)

  implicit val publicKeyOrdering: Ordering[PublicKey] = Ordering.by[PublicKey, Array[Byte]](_.bytes)

}
