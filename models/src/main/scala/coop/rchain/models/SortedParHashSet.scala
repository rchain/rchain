package coop.rchain.models

import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.models.rholang.sorter.ordering._
import cats.Eval

import scala.collection.GenSet
import scala.collection.immutable.HashSet
import coop.rchain.catscontrib.effect.implicits.sEval

//Enforce ordering and uniqueness.
// - uniqueness is handled by using HashSet.
// - ordering comes from sorting the elements prior to serializing.
final class SortedParHashSet(ps: HashSet[Par]) extends Iterable[Par] {

  lazy val sortedPars: List[Par]          = ps.toList.sort
  private lazy val sortedPs: HashSet[Par] = HashSet(sortedPars: _*)

  def +(elem: Par): SortedParHashSet = SortedParHashSet(ps + sort(elem))

  def -(elem: Par): SortedParHashSet = SortedParHashSet(ps - sort(elem))

  def contains(elem: Par): Boolean = sortedPs.contains(sort(elem))

  def union(that: Set[Par]): SortedParHashSet = SortedParHashSet(sortedPs.union(that.map(sort)))

  override def empty: SortedParHashSet = SortedParHashSet(HashSet.empty[Par])

  def iterator: Iterator[Par] = sortedPars.iterator

  override def equals(that: Any): Boolean = that match {
    case sph: SortedParHashSet => sph.sortedPars == this.sortedPars
    case _                     => false
  }

  override def hashCode(): Int = sortedPars.hashCode()

  private def sort(par: Par): Par = Sortable[Par].sortMatch[Eval](par).map(_.term).value
}

object SortedParHashSet {
  def apply(seq: Seq[Par]): SortedParHashSet = new SortedParHashSet(HashSet[Par](seq: _*))

  def apply(set: Set[Par]): SortedParHashSet = SortedParHashSet(set.toSeq)

  def empty: SortedParHashSet = SortedParHashSet(HashSet.empty[Par])
}
