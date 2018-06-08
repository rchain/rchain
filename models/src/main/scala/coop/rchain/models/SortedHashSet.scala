package coop.rchain.models

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.HashSet
import scala.collection.{mutable, SetLike}

//Enforce ordering and uniqueness.
// - uniqueness is handled by using HashSet.
// - ordering comes from sorting the elements prior to serializing.
case class SortedHashSet[A: Ordering](private val ps: HashSet[A])
    extends Set[A]
    with SetLike[A, SortedHashSet[A]] {

  lazy val sortedPars: List[A] = ps.toList.sorted

  override def equals(that: Any): Boolean = that match {
    case shs: SortedHashSet[A] => shs.sortedPars == this.sortedPars
    case _                     => false
  }

  override def hashCode(): Int = sortedPars.hashCode()

  override def empty: SortedHashSet[A] = SortedHashSet.empty[A]

  override def contains(elem: A): Boolean = ps.contains(elem)

  override def +(elem: A): SortedHashSet[A] = SortedHashSet(ps + elem)

  override def -(elem: A): SortedHashSet[A] = SortedHashSet(ps - elem)

  override def iterator: Iterator[A] = sortedPars.toIterator

}

object SortedHashSet {
  def apply[A: Ordering](seq: Seq[A]): SortedHashSet[A] = SortedHashSet(HashSet[A](seq: _*))
  def apply[A: Ordering](): SortedHashSet[A]            = SortedHashSet(HashSet.empty[A])

  def empty[A: Ordering]: SortedHashSet[A] = apply[A]()
}
