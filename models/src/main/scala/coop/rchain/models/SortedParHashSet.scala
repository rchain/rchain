package coop.rchain.models

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.HashSet
import scala.collection.{mutable, SetLike}
import coop.rchain.models.rholang.sort.ordering._

//Enforce ordering and uniqueness.
// - uniqueness is handled by using HashSet.
// - ordering comes from sorting the elements prior to serializing.
case class SortedParHashSet(private val ps: HashSet[Par])
    extends Set[Par]
    with SetLike[Par, SortedParHashSet] {

  lazy val sortedPars: List[Par] = ps.toList.sort

  override def equals(that: Any): Boolean = that match {
    case shs: SortedParHashSet => shs.sortedPars == this.sortedPars
    case _                     => false
  }

  override def hashCode(): Int = sortedPars.hashCode()

  override def empty: SortedParHashSet = SortedParHashSet.empty

  override def contains(elem: Par): Boolean = ps.contains(elem)

  override def +(elem: Par): SortedParHashSet = SortedParHashSet(ps + elem)

  override def -(elem: Par): SortedParHashSet = SortedParHashSet(ps - elem)

  override def iterator: Iterator[Par] = sortedPars.toIterator

}

object SortedParHashSet {
  def apply(seq: Seq[Par]): SortedParHashSet = SortedParHashSet(HashSet[Par](seq: _*))
  def apply(): SortedParHashSet              = SortedParHashSet(HashSet.empty[Par])

  def empty: SortedParHashSet = apply()
}
