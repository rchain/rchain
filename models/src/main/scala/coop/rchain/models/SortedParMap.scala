package coop.rchain.models

import coop.rchain.models.rholang.sorter
import monix.eval.Coeval

import scala.collection.{mutable, GenTraversableOnce}
import scala.collection.immutable.TreeMap
import coop.rchain.models.rholang.sorter.ScoredTerm.{ordering => ScoredTermOrdering}
import coop.rchain.models.rholang.sorter.Sortable

final case class SortedParMap(ps: TreeMap[Par, Par]) extends Iterable[(Par, Par)] {

  def +(kv: (Par, Par)): SortedParMap = SortedParMap(ps + (sort(kv._1) -> sort(kv._2)))

  def ++(kvs: GenTraversableOnce[(Par, Par)]): SortedParMap =
    SortedParMap(ps ++ kvs.toStream.map(kv => sort(kv._1) -> sort(kv._2)))

  def -(key: Par): SortedParMap = SortedParMap(ps - sort(key))

  def --(keys: GenTraversableOnce[Par]): SortedParMap =
    SortedParMap(keys.foldLeft(ps) { (map, key) =>
      map - sort(key)
    })

  def apply(par: Par): Par = ps(sort(par))

  def contains(par: Par): Boolean = ps.contains(sort(par))

  def empty: SortedParMap = SortedParMap.empty

  def get(key: Par): Option[Par] = ps.get(sort(key))

  def getOrElse(key: Par, default: Par): Par = ps.getOrElse(sort(key), default)

  def iterator: Iterator[(Par, Par)] = ps.toIterator

  def keys: Iterable[Par] = ps.keys

  def values: Iterable[Par] = ps.values

  override def head: (Par, Par) = ps.head

  private def sort(par: Par): Par = SortedParMap.scoredTerm(par).term
}

object SortedParMap {
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
  var cacheMiss = 0
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
  var cacheAll = 0

  // From https://stackoverflow.com/a/36960228/2750819
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = {
      cacheAll = cacheAll + 1
      println(s"Cache miss: $cacheMiss, Cache all: $cacheAll")
      getOrElseUpdate(key, f(key))
    }
  }

  lazy val scoredTerm: Par => sorter.ScoredTerm[Par] = memoize { par =>
    cacheMiss = cacheMiss + 1
    cacheAll = cacheAll + 1
    Sortable[Par].sortMatch[Coeval](par).value()
  }

  val parOrdering: Ordering[Par] = new Ordering[Par] {
    override def compare(x: Par, y: Par): Int =
      ScoredTermOrdering[Par].compare(scoredTerm(x), scoredTerm(y))
  }

  def apply(treeMap: TreeMap[Par, Par]): SortedParMap = new SortedParMap(treeMap)

  def apply(map: Map[Par, Par]): SortedParMap =
    SortedParMap(TreeMap.empty[Par, Par](parOrdering) ++ map)

  def apply(seq: Seq[(Par, Par)]): SortedParMap = SortedParMap(seq.toMap)

  def empty: SortedParMap = SortedParMap(TreeMap.empty[Par, Par](parOrdering))
}
