package coop.rchain.models

import scala.collection.immutable.BitSet

trait HasLocallyFree[T] {

  /** Return true if a connective (including free variables and wildcards) is
    *  used anywhere in {@code source}.
    *  @param source the object in question
    *  Specifically looks for constructions that make a pattern non-concrete.
    *  A non-concrete pattern cannot be viewed as if it were a term.
    */
  def connectiveUsed(source: T): Boolean

  /** Returns a bitset representing which variables are locally free if the term
    *  is located at depth {@code depth}
    *  @param source the object in question
    *  @param depth pattern nesting depth
    *  This relies on cached values based on the actual depth of a term and will
    *  only return the correct answer if asked about the actual depth of a term.
    *  The reason we pass depth is that building the caches calls this API and for
    *  the few instances where we don't rely on the cache, we need to know the
    *  depth.
    *
    *  Depth is related to pattern nesting. A top level term is depth 0. A pattern
    *  in a top-level term is depth 1. A pattern in a depth 1 term is depth 2,
    *  etc.
    */
  def locallyFree(source: T, depth: Int): BitSet
}

object HasLocallyFree {
  def apply[T](implicit ev: HasLocallyFree[T]) = ev

  implicit def forTuple[A: HasLocallyFree, B: HasLocallyFree]: HasLocallyFree[(A, B)] =
    new HasLocallyFree[(A, B)] {
      override def connectiveUsed(source: (A, B)): Boolean =
        HasLocallyFree[A].connectiveUsed(source._1) || HasLocallyFree[B].connectiveUsed(source._2)

      override def locallyFree(source: (A, B), depth: Int): BitSet =
        HasLocallyFree[A].locallyFree(source._1, depth) | HasLocallyFree[B]
          .locallyFree(source._2, depth)
    }
}
