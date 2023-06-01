package coop.rchain.casper.util

object Sorting {
  import scala.Ordering.Implicits._
  implicit val byteArrayOrdering = Ordering.by((_: Array[Byte]).toSeq)
}
