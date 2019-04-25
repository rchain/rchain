package coop.rchain.shared

sealed trait StoreType
object StoreType {
  case object Mixed   extends StoreType
  case object LMDB    extends StoreType
  case object InMem   extends StoreType
  case object RSpace2 extends StoreType

  def from(s: String): Option[StoreType] =
    s match {
      case "mixed" => Some(Mixed)
      case "inmem" => Some(InMem)
      case "lmdb"  => Some(LMDB)
      case "v2"    => Some(RSpace2)
      case _       => None
    }

  def toConfig(s: StoreType): String =
    s match {
      case Mixed   => "mixed"
      case LMDB    => "lmdb"
      case InMem   => "inmem"
      case RSpace2 => "v2"
    }
}
