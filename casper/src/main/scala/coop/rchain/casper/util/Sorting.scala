package coop.rchain.casper.util

import coop.rchain.crypto.codec.Base16

object Sorting {

  implicit val byteArrayOrdering = Ordering.by(Base16.encode)
}
