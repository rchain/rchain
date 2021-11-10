package coop.rchain.casper.v2.core.syntax

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.v2.core.SafetyOracle._
import coop.rchain.casper.v2.core._
import coop.rchain.casper.v2.core.syntax.all._
import fs2.Stream

import scala.collection.mutable

trait SafetyOracleSyntax {
  implicit final def safetyOracleSyntax[F[_], M, S](
      c: SafetyOracle[F, M, S]
  ): SafetyOracleOps[F, M, S] = new SafetyOracleOps[F, M, S](c)
}

final class SafetyOracleOps[F[_], M, S](val o: SafetyOracle[F, M, S]) extends AnyVal {
  import o._

  /**
    * @return Stream of fault tolerances that is a result of accumulating agreements stream.
    *         Messages can be repeating, but with growing fault tolerance.
    *         Stream preserves the notion of distance from agreeing messages.
    */
  def faultTolerances(
      agreeingMessages: Set[M],
      dag: DependencyGraph[F, M, S]
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, List[(M, Float)]] = {
    val visited = mutable.TreeMap.empty[M, List[M]]
    dag
      .messagesView(agreeingMessages)(
        m => dag.justifications(m).map(_ diff visited.getOrElse(m, List()))
      )
      .mapAccumulate(Map.empty[M, Set[Agreement[M]]]) { (acc, chunk) =>
        val newAcc = chunk.foldLeft(acc) {
          case (lvlAcc, (visitor, targets)) =>
            visited.update(visitor, targets)
            targets.foldLeft(lvlAcc) { (visitorAcc, target) =>
              if (compatible(visitor, target)) {
                visitorAcc.updated(
                  target,
                  visitorAcc.getOrElse(target, Set()) + Agreement(visitor, target)
                )
              } else visitorAcc
            }
        }
        val chunkTargets = chunk.toList.flatMap { case (_, targets) => targets }
        val out = newAcc.filterKeys(chunkTargets.contains).map {
          case (target, agreements) =>
            (target, SafetyOracle.faultTolerance(agreements, bondsMap, dag.sender))
        }
        (newAcc, out)
      }
      .map { case (_, v) => v.toList.sortBy { case (m, _) => m } }
  }
}
