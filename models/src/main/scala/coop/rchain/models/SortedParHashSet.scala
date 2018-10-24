package coop.rchain.models

import coop.rchain.models.rholang.sorter.ordering._

import scala.collection.SetLike
import scala.collection.immutable.HashSet

//Enforce ordering and uniqueness.
// - uniqueness is handled by using HashSet.
// - ordering comes from sorting the elements prior to serializing.
class SortedParHashSet(private val ps: HashSet[Par])
    extends Set[Par]
    with SetLike[Par, SortedParHashSet] {

  lazy val sortedPars: List[Par] = ps.toList.sort

  override def empty: SortedParHashSet = new SortedParHashSet(HashSet.empty[Par])

  override def contains(elem: Par): Boolean = sortedPars.contains(elem)

  override def +(elem: Par): SortedParHashSet = new SortedParHashSet(ps + elem)

  override def -(elem: Par): SortedParHashSet = new SortedParHashSet(ps - elem)

  override def iterator: Iterator[Par] = sortedPars.toIterator
}

object SortedParHashSet {
  def apply(seq: Seq[Par]): SortedParHashSet = new SortedParHashSet(HashSet[Par](seq: _*))

  def empty: SortedParHashSet = new SortedParHashSet(HashSet.empty[Par])
}
