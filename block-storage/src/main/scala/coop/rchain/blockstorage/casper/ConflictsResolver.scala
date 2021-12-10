package coop.rchain.blockstorage.casper
import coop.rchain.blockstorage.casper.ConflictsResolver.ConflictResolution

/** @tparam U Minimal rejection unit. */
trait ConflictsResolver[F[_], U] {
  def resolve(conflictSet: Set[U], toEnforce: ConflictResolution[U]): F[ConflictResolution[U]]
  def clean(conflictSet: Set[U], finalizedSet: Set[U]): F[Set[U]]
}

object ConflictsResolver {
  final case class ConflictResolution[U](acceptedSet: Set[U], rejectedSet: Set[U])
}
