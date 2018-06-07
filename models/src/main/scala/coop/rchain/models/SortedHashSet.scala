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

  // -- METHODS USED BY PROTOBUF SERIALIZATION MECHANISM, ADJUST WITH CARE! --
  // Called inside `writeTo` when serializing instance.
  // Elements should be already sorted so `foreach` traverse them in order.
  override def foreach[U](f: A => U): Unit = sortedPars.foreach(f)

  override def map[B, That](f: A => B)(
      implicit cbf: CanBuildFrom[SortedHashSet[A], B, That]): That = {
    val b = cbf.apply()
    sortedPars.foreach(x => b += f(x))
    b.result()
  }

  def ++(s: TraversableOnce[A]): SortedHashSet[A] = SortedHashSet(ps ++ s)
  // -- END --

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

  // -- Required by scalapb --
  implicit def cbf[From, A: Ordering](
      implicit vcbf: CanBuildFrom[From, A, Seq[A]]): CanBuildFrom[From, A, SortedHashSet[A]] =
    new CanBuildFrom[From, A, SortedHashSet[A]] {
      override def apply(from: From): mutable.Builder[A, SortedHashSet[A]] =
        vcbf(from).mapResult(res => SortedHashSet(HashSet[A](res: _*)))

      override def apply(): mutable.Builder[A, SortedHashSet[A]] =
        vcbf().mapResult(_ => SortedHashSet[A]())
    }

  class Builder[A: Ordering] {
    private val underlying = HashSet.newBuilder[A]

    def ++=(other: SortedHashSet[A]): Builder[A] = {
      underlying ++= other.ps
      this
    }

    def +=(other: A): Unit = underlying += other

    def result(): SortedHashSet[A] = SortedHashSet[A](underlying.result())
  }

  def newBuilder[A: Ordering] = new Builder[A]
}
