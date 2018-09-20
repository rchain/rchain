package coop.rchain.rholang.interpreter.storage

import cats.Parallel
import cats.effect.Sync
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.rholang.interpreter.Dispatch
import coop.rchain.rholang.interpreter.errors.{OutOfPhlogistonsError, ReduceError}
import coop.rchain.rspace.pure.PureRSpace
import cats.implicits._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount}
import coop.rchain.rholang.interpreter.storage.implicits._

trait TuplespaceAlg[F[_]] {
  def produce(chan: Channel, data: ListChannelWithRandom, persistent: Boolean): F[CostAccount]
  def consume(binds: Seq[(BindPattern, Quote)],
              body: ParWithRandom,
              persistent: Boolean): F[CostAccount]
}

object TuplespaceAlg {
  def rspaceTuplespace[F[_], M[_]](
      pureRSpace: PureRSpace[F,
                             Channel,
                             BindPattern,
                             OutOfPhlogistonsError.type,
                             ListChannelWithRandom,
                             ListChannelWithRandom,
                             TaggedContinuation],
      dispatcher: => Dispatch[F, ListChannelWithRandom, TaggedContinuation])(
      implicit F: Sync[F],
      P: Parallel[F, M]): TuplespaceAlg[F] = new TuplespaceAlg[F] {
    override def produce(channel: Channel,
                         data: ListChannelWithRandom,
                         persistent: Boolean): F[CostAccount] = {
      // TODO: Handle the environment in the store
      def go(
          res: Either[OutOfPhlogistonsError.type,
                      Option[(TaggedContinuation, Seq[ListChannelWithRandom])]]): F[CostAccount] =
        res match {
          case Right(Some((continuation, dataList))) =>
            val rspaceMatchCost =
              dataList
                .map(_.cost.map(CostAccount.fromProto(_)).getOrElse(CostAccount(0)))
                .toList
                .map(ca => ca.copy(cost = Cost(Integer.MAX_VALUE) - ca.cost))
                .combineAll
            if (persistent) {
              List(dispatcher.dispatch(continuation, dataList) *> F.pure(CostAccount(0)),
                   produce(channel, data, persistent)).parSequence
                .map(_.combineAll + rspaceMatchCost)
            } else {
              dispatcher.dispatch(continuation, dataList) *> rspaceMatchCost.pure[F]
            }

          case Right(None) => F.pure(CostAccount(0))
        }

      for {
        res  <- pureRSpace.produce(channel, data, persist = persistent)
        cost <- go(res)
      } yield cost
    }

    override def consume(binds: Seq[(BindPattern, Quote)],
                         body: ParWithRandom,
                         persistent: Boolean): F[CostAccount] =
      binds match {
        case Nil => F.raiseError(ReduceError("Error: empty binds"))
        case _ =>
          val (patterns: Seq[BindPattern], sources: Seq[Quote]) = binds.unzip
          def go(
              res: Either[OutOfPhlogistonsError.type,
                          Option[(TaggedContinuation, Seq[ListChannelWithRandom])]])
            : F[CostAccount] =
            res match {
              case Right(Some((continuation, dataList))) =>
                val rspaceMatchCost =
                  dataList
                    .map(
                      _.cost
                        .map(CostAccount.fromProto(_))
                        .getOrElse(CostAccount(0)))
                    .toList
                    .map(ca => ca.copy(cost = Cost(Integer.MAX_VALUE) - ca.cost))
                    .combineAll

                dispatcher.dispatch(continuation, dataList)
                if (persistent) {
                  List(dispatcher.dispatch(continuation, dataList) *> F.pure(CostAccount(0)),
                       consume(binds, body, persistent)).parSequence
                    .map(_.combineAll + rspaceMatchCost)
                } else {
                  dispatcher.dispatch(continuation, dataList) *> rspaceMatchCost.pure[F]
                }
              case Right(None) => F.pure(CostAccount(0))
            }

          for {
            res <- pureRSpace.consume(sources.map(q => Channel(q)).toList,
                                      patterns.toList,
                                      TaggedContinuation(ParBody(body)),
                                      persist = persistent)
            cost <- go(res)
          } yield cost
      }
  }
}
