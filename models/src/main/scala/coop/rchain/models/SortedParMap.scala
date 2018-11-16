package coop.rchain.models

import coop.rchain.models.rholang.sorter.ordering._

import scala.collection.GenTraversableOnce
import scala.collection.immutable.HashMap

final class SortedParMap private (ps: Map[Par, Par]) extends Iterable[(Par, Par)] {

  // TODO: Merge `sortedList` and `sortedMap` into one VectorMap once available
  lazy val sortedList: List[(Par, Par)] = ps.sort
  lazy val sortedMap: HashMap[Par, Par] = HashMap(sortedList: _*)

  def +(kv: (Par, Par)): SortedParMap = SortedParMap(sortedMap + kv)

  def ++(kvs: GenTraversableOnce[(Par, Par)]): SortedParMap = SortedParMap(sortedMap ++ kvs)

  def -(key: Par): SortedParMap = SortedParMap(sortedMap - key.sort)

  def --(keys: GenTraversableOnce[Par]): SortedParMap =
    SortedParMap(keys.foldLeft(sortedMap) { (map, kv) =>
      map - kv.sort
    })

  def apply(par: Par): Par = sortedMap(par.sort)

  def contains(par: Par): Boolean = sortedMap.contains(par.sort)

  def empty: SortedParMap = SortedParMap(Map.empty[Par, Par])

  def get(key: Par): Option[Par] = sortedMap.get(key.sort)

  def getOrElse(key: Par, default: Par): Par = sortedMap.getOrElse(key.sort, default)

  def iterator: Iterator[(Par, Par)] = sortedList.toIterator

  def keys: Iterable[Par] = sortedList.map(_._1)

  def values: Iterable[Par] = sortedList.map(_._2)

  override def equals(that: Any): Boolean = that match {
    case spm: SortedParMap => spm.sortedList == this.sortedList
    case _                 => false
  }

  override def hashCode(): Int = sortedList.hashCode()
}

object SortedParMap {
  def apply(map: Map[Par, Par]): SortedParMap = new SortedParMap(map)

  def apply(seq: Seq[(Par, Par)]): SortedParMap = SortedParMap(seq.toMap)

  def empty: SortedParMap = SortedParMap(Map.empty[Par, Par])
}
