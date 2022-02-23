package coop.rchain
import coop.rchain.pcasper.finalization.Fringe

package object pcasper {

  /** Find senders that should be ejected. */
  def findLazy[M, S](latestFringes: Iterator[Fringe[M, S]], latenessTolerance: Long): Iterator[S] =
    ???
}
