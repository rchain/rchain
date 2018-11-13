package coop.rchain.models

import coop.rchain.models.rholang.sorter.ordering._

import scala.collection.GenTraversableOnce
import scala.collection.immutable.ListMap

final class SortedParMap private (ps: Map[Par, Par]) extends Iterable[(Par, Par)] {

  // TODO: Merge `sortedMap` and `sortedMapLookup` into one VectorMap once available
  lazy val sortedMap: ListMap[Par, Par]   = ListMap(ps.sort: _*)
  lazy val sortedMapLookup: Map[Par, Par] = ps.sort.toMap

  def +(kv: (Par, Par)): SortedParMap = SortedParMap(sortedMap + kv)

  def ++(kvs: GenTraversableOnce[(Par, Par)]): SortedParMap = SortedParMap(sortedMap ++ kvs)

  def -(key: Par): SortedParMap = SortedParMap(sortedMap - key)

  def --(keys: GenTraversableOnce[Par]): SortedParMap = SortedParMap(sortedMap -- keys)

  def apply(par: Par): Par = sortedMapLookup(par)

  def contains(par: Par): Boolean = sortedMapLookup.contains(par)

  def empty: SortedParMap = SortedParMap(Map.empty[Par, Par])

  def get(key: Par): Option[Par] = sortedMapLookup.get(key)

  def getOrElse(key: Par, default: Par): Par = sortedMapLookup.getOrElse(key, default)

  def iterator: Iterator[(Par, Par)] = sortedMap.toIterator

  def keys: Iterable[Par] = sortedMap.keys

  def values: Iterable[Par] = sortedMap.values

  override def equals(that: Any): Boolean = that match {
    case spm: SortedParMap => spm.sortedMap == this.sortedMap
    case _                 => false
  }

  override def hashCode(): Int = sortedMap.hashCode()
}

object SortedParMap {
  def apply(map: Map[Par, Par]): SortedParMap = new SortedParMap(map)

  def apply(seq: Seq[(Par, Par)]): SortedParMap = SortedParMap(seq.toMap)

  def apply(sortedParMap: SortedParMap): SortedParMap = SortedParMap(sortedParMap.sortedMap)

  def empty: SortedParMap = SortedParMap(Map.empty[Par, Par])
}
