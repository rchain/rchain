package coop.rchain.rholang.interpreter

import cats.MonadError
import coop.rchain.models.{Channel, ReceiveBind, Var}
import coop.rchain.models.rholang.sort.ReceiveSortMatcher.sortBind
import coop.rchain.rholang.interpreter.errors.{InterpreterError, SortMatchError}
import cats.implicits._
import coop.rchain.models.rholang.sort._
import coop.rchain.models.rholang.implicits._

object ReceiveBindsSortMatcher {
  // Used during normalize to presort the binds.
  def preSortBinds[M[_], T](binds: Seq[(Seq[Channel], Channel, Option[Var], DebruijnLevelMap[T])])(
      implicit err: MonadError[M, InterpreterError]): M[Seq[(ReceiveBind, DebruijnLevelMap[T])]] = {
    val sortedBind = binds.toList
      .traverse {
        case (patterns: Seq[Channel],
              channel: Channel,
              remainder: Option[Var],
              knownFree: DebruijnLevelMap[T]) =>
          sortBind(
            ReceiveBind(patterns, channel, remainder, freeCount = knownFree.countNoWildcards))
            .map(sortedBind => ScoredTerm((sortedBind.term, knownFree), sortedBind.score))
      }
      .map(_.sorted)

    err.fromEither(sortedBind.leftMap(th => SortMatchError(th.getMessage))).map(_.map(_.term))
  }
}
