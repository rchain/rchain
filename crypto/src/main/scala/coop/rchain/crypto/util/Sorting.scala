package coop.rchain.crypto.util

import coop.rchain.crypto.PublicKey

import scala.Ordering.Implicits._

object Sorting {

  implicit val byteArrayOrdering = Ordering.by((_: Array[Byte]).toSeq)

  implicit val publicKeyOrdering: Ordering[PublicKey] = Ordering.by[PublicKey, Array[Byte]](_.bytes)

}
