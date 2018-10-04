package coop.rchain.shared

sealed trait StoreType
object StoreType {
  case object Mixed extends StoreType
  case object LMDB  extends StoreType
  case object InMem extends StoreType

  def from(s: String): Option[StoreType] = s match {
    case "mixed" => Some(Mixed)
    case "inmem" => Some(InMem)
    case "lmdb"  => Some(LMDB)
    case _       => None
  }
}
