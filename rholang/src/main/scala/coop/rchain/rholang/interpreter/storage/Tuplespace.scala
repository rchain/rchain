package coop.rchain.rholang.interpreter.storage

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Dispatch
import coop.rchain.rholang.interpreter.errors.{InterpreterError, ReduceError}
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.rspace.util._

trait Tuplespace[F[_]] {
  def produce(chan: Par, data: ListParWithRandom, persistent: Boolean, sequenceNumber: Int): F[Unit]
  def consume(
      binds: Seq[(BindPattern, Par)],
      body: ParWithRandom,
      persistent: Boolean,
      sequenceNumber: Int
  ): F[Unit]
}

object Tuplespace {
  def rspaceTuplespace[F[_], M[_]](
      pureRSpace: PureRSpace[
        F,
        Par,
        BindPattern,
        InterpreterError,
        ListParWithRandom,
        ListParWithRandom,
        TaggedContinuation
      ],
      dispatcher: => Dispatch[F, ListParWithRandom, TaggedContinuation]
  )(implicit F: Sync[F], P: Parallel[F, M]): Tuplespace[F] = new Tuplespace[F] {
    override def produce(
        channel: Par,
        data: ListParWithRandom,
        persistent: Boolean,
        sequenceNumber: Int
    ): F[Unit] = {
      // TODO: Handle the environment in the store
      def go(
          res: Option[(TaggedContinuation, Seq[ListParWithRandom], Int)]
      ): F[Unit] =
        res match {
          case Some((continuation, dataList, updatedSequenceNumber)) =>
            if (persistent) {
              Parallel
                .parProduct(
                  dispatcher.dispatch(continuation, dataList, updatedSequenceNumber),
                  produce(channel, data, persistent, sequenceNumber)
                )
                .as(())
            } else {
              dispatcher.dispatch(continuation, dataList, updatedSequenceNumber)
            }
          case None => F.unit
        }

      for {
        res <- pureRSpace.produce(channel, data, persist = persistent, sequenceNumber)
        _   <- go(res)
      } yield ()
    }

    override def consume(
        binds: Seq[(BindPattern, Par)],
        body: ParWithRandom,
        persistent: Boolean,
        sequenceNumber: Int
    ): F[Unit] =
      binds match {
        case Nil => F.raiseError(ReduceError("Error: empty binds"))
        case _ =>
          val (patterns: Seq[BindPattern], sources: Seq[Par]) = binds.unzip
          def go(
              res: Option[(TaggedContinuation, Seq[ListParWithRandom], Int)]
          ): F[Unit] =
            res match {
              case Some((continuation, dataList, updatedSequenceNumber)) =>
                if (persistent) {
                  Parallel
                    .parProduct(
                      dispatcher.dispatch(continuation, dataList, updatedSequenceNumber),
                      consume(binds, body, persistent, sequenceNumber)
                    )
                    .as(())
                } else {
                  dispatcher.dispatch(continuation, dataList, updatedSequenceNumber)
                }
              case None => F.unit
            }

          for {
            res <- pureRSpace.consume(
                    sources.toList,
                    patterns.toList,
                    TaggedContinuation(ParBody(body)),
                    persist = persistent,
                    sequenceNumber
                  )
            _ <- go(res)
          } yield ()
      }
  }
}
