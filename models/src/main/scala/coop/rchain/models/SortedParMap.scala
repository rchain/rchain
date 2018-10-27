package coop.rchain.models

import scala.collection.MapLike
import coop.rchain.models.rholang.sorter.ordering._

class SortedParMap private (private val ps: Map[Par, Par])
    extends Map[Par, Par]
    with MapLike[Par, Par, SortedParMap] {

  lazy val sortedMap: Map[Par, Par] = ps.sort.toMap

  override def empty: SortedParMap = SortedParMap(Map.empty[Par, Par])

  override def get(key: Par): Option[Par] = sortedMap.get(key)

  override def iterator: Iterator[(Par, Par)] = sortedMap.toIterator

  override def +[V1 >: Par](kv: (Par, V1)): Map[Par, V1] = sortedMap + kv

  override def -(key: Par): SortedParMap = SortedParMap(sortedMap - key)
}

object SortedParMap {
  def apply(map: Map[Par, Par]): SortedParMap = new SortedParMap(map)

  def apply(seq: Seq[(Par, Par)]): SortedParMap = SortedParMap(seq.toMap)

  def empty: SortedParMap = SortedParMap(Map.empty[Par, Par])
}
