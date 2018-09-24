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
import coop.rchain.rholang.interpreter.storage.implicits.matchListQuote

trait TuplespaceAlg[F[_]] {
  def produce(chan: Channel, data: ListChannelWithRandom, persistent: Boolean): F[Unit]
  def consume(
      binds: Seq[(BindPattern, Quote)],
      body: ParWithRandom,
      persistent: Boolean
  ): F[Unit]
}

object TuplespaceAlg {
  def rspaceTuplespace[F[_], M[_]](
      pureRSpace: PureRSpace[
        F,
        Channel,
        BindPattern,
        OutOfPhlogistonsError.type,
        ListChannelWithRandom,
        ListChannelWithRandomAndPhlos,
        TaggedContinuation
      ],
      dispatcher: => Dispatch[F, ListChannelWithRandomAndPhlos, TaggedContinuation]
  )(implicit F: Sync[F], P: Parallel[F, M]): TuplespaceAlg[F] = new TuplespaceAlg[F] {
    override def produce(
        channel: Channel,
        data: ListChannelWithRandom,
        persistent: Boolean
    ): F[Unit] = {
      // TODO: Handle the environment in the store
      def go(
          res: Either[OutOfPhlogistonsError.type, Option[
            (TaggedContinuation, Seq[ListChannelWithRandomAndPhlos])
          ]]
      ): F[Unit] =
        res match {
          case Left(oope) => F.raiseError(oope)
          case Right(Some((continuation, dataList))) =>
            if (persistent) {
              Parallel
                .parProduct(
                  dispatcher.dispatch(continuation, dataList),
                  produce(channel, data, persistent)
                )
                .as(())
            } else {
              dispatcher.dispatch(continuation, dataList)
            }

          case Right(None) => F.unit
        }

      for {
        res <- pureRSpace.produce(channel, data, persist = persistent)
        _   <- go(res)
      } yield ()
    }

    override def consume(
        binds: Seq[(BindPattern, Quote)],
        body: ParWithRandom,
        persistent: Boolean
    ): F[Unit] =
      binds match {
        case Nil => F.raiseError(ReduceError("Error: empty binds"))
        case _ =>
          val (patterns: Seq[BindPattern], sources: Seq[Quote]) = binds.unzip
          def go(
              res: Either[OutOfPhlogistonsError.type, Option[
                (TaggedContinuation, Seq[ListChannelWithRandomAndPhlos])
              ]]
          ): F[Unit] =
            res match {
              case Left(oope) => F.raiseError(oope)
              case Right(Some((continuation, dataList))) =>
                if (persistent) {
                  Parallel
                    .parProduct(
                      dispatcher.dispatch(continuation, dataList),
                      consume(binds, body, persistent)
                    )
                    .as(())
                } else {
                  dispatcher.dispatch(continuation, dataList)
                }
              case Right(None) => F.unit
            }

          for {
            res <- pureRSpace.consume(
                    sources.map(q => Channel(q)).toList,
                    patterns.toList,
                    TaggedContinuation(ParBody(body)),
                    persist = persistent
                  )
            _ <- go(res)
          } yield ()
      }
  }
}
