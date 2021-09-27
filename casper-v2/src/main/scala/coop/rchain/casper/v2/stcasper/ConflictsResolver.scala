package coop.rchain.casper.v2.stcasper
import coop.rchain.casper.v2.stcasper.ConflictsResolver.ConflictResolution

/** @tparam U Minimal rejection unit. */
trait ConflictsResolver[F[_], U] { def resolve(conflictSet: Set[U]): F[ConflictResolution[U]] }

object ConflictsResolver {
  final case class ConflictResolution[U](acceptedSet: Set[U], rejectedSet: Set[U])
}
