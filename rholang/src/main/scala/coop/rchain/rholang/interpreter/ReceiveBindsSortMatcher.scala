package coop.rchain.rholang.interpreter

import cats.effect.Sync
import coop.rchain.models.{Par, ReceiveBind, Var}
import coop.rchain.models.rholang.sorter.ReceiveSortMatcher.sortBind
import cats.implicits._
import coop.rchain.models.rholang.sorter._
import coop.rchain.models.rholang.implicits._

object ReceiveBindsSortMatcher {
  // Used during normalize to presort the binds.
  def preSortBinds[F[_]: Sync, T](
      binds: Seq[(Seq[Par], Par, Option[Var], DebruijnLevelMap[T])]
  ): F[Seq[(ReceiveBind, DebruijnLevelMap[T])]] = {
    val bindSortings = binds.toList
      .map {
        case (
            patterns: Seq[Par],
            channel: Par,
            remainder: Option[Var],
            knownFree: DebruijnLevelMap[T]
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
