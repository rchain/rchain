package coop.rchain.models

import coop.rchain.models.rholang.sorter.Sortable

import monix.eval.Coeval

import scala.collection.GenTraversableOnce
import scala.collection.immutable.TreeMap

import coop.rchain.models.rholang.sorter.ScoredTerm.{ordering => ScoredTermOrdering}

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

  private def sort(par: Par): Par = Sortable[Par].sortMatch[Coeval](par).map(_.term).value()
}

object SortedParMap {
  implicit def ordering: Ordering[Par] = (x: Par, y: Par) => {
    (for {
      termX <- Sortable[Par].sortMatch[Coeval](x)
      termY <- Sortable[Par].sortMatch[Coeval](y)
    } yield ScoredTermOrdering.compare(termX, termY)).value()
  }

  def apply(treeMap: TreeMap[Par, Par]): SortedParMap = new SortedParMap(treeMap)

  def apply(map: Map[Par, Par]): SortedParMap =
    SortedParMap(TreeMap.empty[Par, Par](ordering) ++ map)

  def apply(seq: Seq[(Par, Par)]): SortedParMap = SortedParMap(seq.toMap)

  def empty: SortedParMap = SortedParMap(TreeMap.empty[Par, Par](ordering))
}
