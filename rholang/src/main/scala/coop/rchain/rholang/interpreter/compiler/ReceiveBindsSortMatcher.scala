package coop.rchain.rholang.interpreter.compiler

import cats.effect.Sync
import coop.rchain.models.rholang.sorter.ReceiveSortMatcher.sortBind
import coop.rchain.models.{Par, ReceiveBind, Var}
import cats.implicits._
import coop.rchain.models.rholang.sorter._
import coop.rchain.models.rholang.implicits._

object ReceiveBindsSortMatcher {
  // Used during normalize to presort the binds.
  def preSortBinds[F[_]: Sync, T](
      binds: Vector[(Vector[Par], Par, Option[Var], DeBruijnLevelMap[T])]
  ): F[Vector[(ReceiveBind, DeBruijnLevelMap[T])]] = {
    val bindSortings = binds
      .map {
        case (
            patterns: Vector[Par],
            channel: Par,
            remainder: Option[Var],
            knownFree: DeBruijnLevelMap[T]
            ) =>
          for {
            sortedBind <- sortBind(
                           ReceiveBind(
                             patterns,
                             channel,
                             remainder,
                             freeCount = knownFree.countNoWildcards
                           )
                         )
          } yield ScoredTerm((sortedBind.term, knownFree), sortedBind.score)
      }

    for {
      binds <- bindSortings.sequence
    } yield binds.sorted.map(_.term)
  }

}
