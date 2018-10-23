package coop.rchain.models

import scala.collection.MapLike
import coop.rchain.models.rholang.sorter.ordering._

case class SortedParMap(private val ps: Map[Par, Par])
    extends Map[Par, Par]
    with MapLike[Par, Par, SortedParMap] {

  lazy val sortedMap: List[(Par, Par)] = ps.sort

  override def empty: SortedParMap = SortedParMap.empty

  override def get(key: Par): Option[Par] = sortedMap.toMap.get(key)

  override def iterator: Iterator[(Par, Par)] = sortedMap.toIterator

  override def +[V1 >: Par](kv: (Par, V1)): Map[Par, V1] = sortedMap.toMap + kv

  override def -(key: Par): SortedParMap = SortedParMap(sortedMap.toMap - key)

  override def equals(that: Any): Boolean =
    that match {
      case _: SortedParMap => this.sortedMap == that.asInstanceOf[SortedParMap].sortedMap
      case _               => false
    }
}

object SortedParMap {
  def empty: SortedParMap = SortedParMap(Map.empty[Par, Par])

  def apply(seq: Seq[(Par, Par)]): SortedParMap = SortedParMap(seq.toMap)
}
