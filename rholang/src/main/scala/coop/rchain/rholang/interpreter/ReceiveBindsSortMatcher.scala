package coop.rchain.rholang.interpreter

import cats.effect.Sync
import coop.rchain.models.{Channel, ReceiveBind, Var}
import coop.rchain.models.rholang.sort.ReceiveSortMatcher.sortBind
import cats.implicits._
import coop.rchain.models.rholang.sort._
import coop.rchain.models.rholang.implicits._

object ReceiveBindsSortMatcher {
  // Used during normalize to presort the binds.
  def preSortBinds[F[_]: Sync, T](
      binds: Seq[(Seq[Channel], Channel, Option[Var], DebruijnLevelMap[T])]
  ): F[Seq[(ReceiveBind, DebruijnLevelMap[T])]] = {
    val bindSortings = binds.toList
      .map {
        case (
            patterns: Seq[Channel],
            channel: Channel,
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
