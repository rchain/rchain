package coop.rchain.models.rholangn

import cats.Eval
import cats.syntax.all._
import coop.rchain.models.rholangn.ParN._

import scala.collection.immutable.{TreeMap, TreeSet}

/**
  * Ordered collection of 0 or more processes.
  * @param ps The sequence of any Rholang processes
  * @param remainder gives support to use ... in the list construction and deconstruction e.g. [1, 2, 3 ... rest].
  *                  It's defined as optional variable.
  */
final class EListN(val ps: Seq[ParN], val remainder: Option[VarN]) extends CollectionN {
  def :+(elem: ParN): EListN       = EListN(ps :+ elem, remainder)
  def +:(elem: ParN): EListN       = EListN(elem +: ps, remainder)
  def ++(elems: Seq[ParN]): EListN = EListN(ps ++ elems, None)
  def ++(that: EListN): EListN     = EListN(ps ++ that.ps, None)
}

object EListN {
  def apply(ps: Seq[ParN] = Seq(), r: Option[VarN] = None): EListN = new EListN(ps, r)
  def apply(p: ParN): EListN                                       = apply(Seq(p), None)
  def empty: EListN                                                = EListN()
}

/**
  * Ordered collection of 1 or more processes.
  * @param ps The non-empty sequence of any Rholang processes
  */
final class ETupleN private (val ps: Seq[ParN]) extends CollectionN

object ETupleN {
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def apply(ps: Seq[ParN]): ETupleN =
    if (ps.isEmpty) throw new Exception("Cannot create ETuple with an empty par sequence")
    else new ETupleN(ps)
  def apply(p: ParN): ETupleN = apply(Seq(p))
}

/**
  * A Rholang set is an unordered collection of 0 or more processes.
  * @param ps The sequence of any Rholang processes
  * @param remainder gives support to use ... in the set construction and deconstruction e.g. Set(1, 2, 3 ... rest).
  *                  It's defined as optional variable.
  */
final class ESetN(val ps: TreeSet[ParN], val remainder: Option[VarN]) extends CollectionN {
  // Sorted by the hash of the objects which is memoized as part of Rho type
  val psSorted: Eval[Seq[ParN]] = this.ps.toSeq.sortByBytes(_.rhoHash).memoize

  def +(elem: ParN): ESetN = ESetN(ps + elem, remainder)
  def -(elem: ParN): ESetN = ESetN(ps - elem, remainder)

  def ++(elems: Seq[ParN]): ESetN = ESetN(ps ++ elems, None)
  def --(elems: Seq[ParN]): ESetN = ESetN(ps -- elems, None)

  def ++(that: ESetN): ESetN = ESetN(ps ++ that.ps, None)
  def --(that: ESetN): ESetN = ESetN(ps -- that.ps, None)

  def contains(elem: ParN): Boolean = ps.contains(elem)
}

object ESetN {
  def apply(): ESetN = new ESetN(TreeSet.empty(ParN.ordering), None)
  def apply(ps: Seq[ParN], r: Option[VarN] = None): ESetN =
    new ESetN(TreeSet.from(ps)(ParN.ordering), r)
  def apply(p: ParN): ESetN                                            = ESetN(Seq(p), None)
  def empty: ESetN                                                     = ESetN()
  private def apply(ps: TreeSet[ParN], remainder: Option[VarN]): ESetN = new ESetN(ps, remainder)
}

/**
  * A Rholang map is an unordered collection of 0 or more key-value pairs; both keys and values are processes.
  * @param ps The sequence of any Rholang processes (that form key-value pairs)
  * @param remainder gives support to use ... in the set construction and deconstruction e.g. {"a":1, "b":2 ... rest}.
  *                  It's defined as optional variable.
  */
final class EMapN(val ps: TreeMap[ParN, ParN], val remainder: Option[VarN]) extends CollectionN {
  // Sorted by the hash of the objects which is memoized as part of Rho type
  val psSorted: Eval[Seq[(ParN, ParN)]] =
    ps.toSeq.sortByBytes(_.bimap(_.rhoHash, _.rhoHash).mapN(_ ++ _)).memoize

  def +(kv: (ParN, ParN)): EMapN = EMapN(ps + kv, remainder)
  def -(key: ParN): EMapN        = EMapN(ps - key, remainder)

  def ++(kvs: Seq[(ParN, ParN)]): EMapN = EMapN(ps ++ kvs, None)
  def --(keys: Iterable[ParN]): EMapN   = EMapN(ps -- keys, None)

  def ++(that: EMapN): EMapN = EMapN(ps ++ that.ps, None)
  def --(that: EMapN): EMapN = EMapN(ps -- that.keys, None)

  def contains(p: ParN): Boolean                = ps.contains(p)
  def get(key: ParN): Option[ParN]              = ps.get(key)
  def getOrElse(key: ParN, default: ParN): ParN = ps.getOrElse(key, default)

  def keys: Seq[ParN]   = ps.keys.toSeq
  def values: Seq[ParN] = ps.values.toSeq
}

object EMapN {
  def apply(ps: Seq[(ParN, ParN)], r: Option[VarN]): EMapN =
    new EMapN(TreeMap.from(ps)(ParN.ordering), r)
  def apply(ps: Seq[(ParN, ParN)]): EMapN = apply(ps, None)

  def apply(ps: Map[ParN, ParN], r: Option[VarN]): EMapN =
    new EMapN(TreeMap.from(ps)(ParN.ordering), r)
  def apply(ps: Map[ParN, ParN]): EMapN = apply(ps, None)

  def apply(): EMapN = apply(Seq())
  def empty: EMapN   = EMapN()

  private def apply(ps: TreeMap[ParN, ParN], remainder: Option[VarN]): EMapN =
    new EMapN(ps, remainder)
}
