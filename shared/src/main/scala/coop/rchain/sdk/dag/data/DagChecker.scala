package coop.rchain.sdk.dag.data

/**
  * Represents interface for DAG manager to maintain latest state of the DAG.
  *
  * @tparam M represents a message
  * @tparam MId represents message unique identifier (hash)
  */
trait DagChecker[F[_], M, MId] {
  def getDependencies(m: M): F[Set[MId]]

  /**
    * Basic block validation (signature, fields, format, ...)
    */
  def check(m: M): F[MessageCheckResult]
}

sealed trait MessageCheckResult
