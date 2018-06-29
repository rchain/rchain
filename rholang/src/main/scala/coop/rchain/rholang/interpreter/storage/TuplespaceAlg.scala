package coop.rchain.rholang.interpreter.storage

import cats.Parallel
import cats.effect.Sync
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.{BindPattern, Channel, Par, TaggedContinuation}
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.rholang.interpreter.Dispatch
import coop.rchain.rholang.interpreter.errors.ReduceError
import coop.rchain.rspace.pure.PureRSpace
import cats.implicits._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.storage.implicits._

trait TuplespaceAlg[F[_]] {
  def produce(chan: Channel, data: Seq[Channel], persistent: Boolean): F[Unit]
  def consume(binds: Seq[(BindPattern, Quote)], body: Par, persistent: Boolean): F[Unit]
}

object TuplespaceAlg {
  def rspaceTuplespace[F[_], M[_]](pureRSpace: PureRSpace[F,
                                                          Channel,
                                                          BindPattern,
                                                          Seq[Channel],
                                                          Seq[Channel],
                                                          TaggedContinuation],
                                   dispatcher: => Dispatch[F, Seq[Channel], TaggedContinuation])(
      implicit F: Sync[F],
      P: Parallel[F, M]): TuplespaceAlg[F] = new TuplespaceAlg[F] {
    override def produce(channel: Channel, data: Seq[Channel], persistent: Boolean): F[Unit] = {
      // TODO: Handle the environment in the store
      def go(res: Option[(TaggedContinuation, Seq[Seq[Channel]])]): F[Unit] =
        res match {
          case Some((continuation, dataList)) =>
            if (persistent) {
              Parallel.parSequence_[List, F, M, Unit](
                List(dispatcher.dispatch(continuation, dataList),
                     produce(channel, data, persistent)))
            } else {
              dispatcher.dispatch(continuation, dataList)
            }
          case None =>
            F.unit
        }

      for {
        res <- pureRSpace.produce(channel, data, persist = persistent)
        _   <- go(res)
      } yield ()
    }

    override def consume(binds: Seq[(BindPattern, Quote)],
                         body: Par,
                         persistent: Boolean): F[Unit] =
      binds match {
        case Nil => F.raiseError(ReduceError("Error: empty binds"))
        case _ =>
          val (patterns: Seq[BindPattern], sources: Seq[Quote]) = binds.unzip
          pureRSpace
            .consume(sources.map(q => Channel(q)).toList,
                     patterns.toList,
                     TaggedContinuation(ParBody(body)),
                     persist = persistent)
            .flatMap {
              case Some((continuation, dataList)) =>
                dispatcher.dispatch(continuation, dataList)
                if (persistent) {
                  List(dispatcher.dispatch(continuation, dataList),
                       consume(binds, body, persistent)).parSequence
                    .as(())
                } else {
                  dispatcher.dispatch(continuation, dataList)
                }
              case None => F.unit
            }
      }
  }
}
