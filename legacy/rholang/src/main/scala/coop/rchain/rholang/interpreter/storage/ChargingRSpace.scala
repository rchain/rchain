package coop.rchain.rholang.interpreter.storage

import java.nio.ByteBuffer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Span
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoTuplespace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.storage.ChargingRSpace.consumeId
import coop.rchain.rspace.{ContResult, Result, Match => StorageMatch}

import scala.collection.SortedSet

object ChargingRSpace {

  private sealed trait TriggeredBy {
    val id: Blake2b512Random
    val persistent: Boolean
    val channelsCount: Int
  }

  private final case class Consume(id: Blake2b512Random, persistent: Boolean, channelsCount: Int)
      extends TriggeredBy

  private final case class Produce(id: Blake2b512Random, persistent: Boolean) extends TriggeredBy {
    override val channelsCount = 1
  }

  private def consumeId[F[_]: Sync](continuation: TaggedContinuation): F[Blake2b512Random] =
    //TODO: Make ScalaBodyRef-s have their own random state and merge it during its COMMs
    continuation.taggedCont match {
      case ParBody(value) => value.randomState.pure[F]
      case ScalaBodyRef(value) =>
        Blake2b512Random(ByteBuffer.allocate(8).putLong(value).array()).pure[F]
      case Empty => BugFoundError("Damn you pROTOBUF").raiseError[F, Blake2b512Random]
    }

  def chargingRSpace[F[_]: Sync: Span](
      space: RhoTuplespace[F]
  )(implicit cost: _cost[F]): RhoTuplespace[F] =
    new RhoTuplespace[F] {

      implicit override val m: StorageMatch[F, BindPattern, ListParWithRandom] = space.m

      override def consume(
          channels: Seq[Par],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation,
          persist: Boolean,
          peeks: SortedSet[Int] = SortedSet.empty[Int]
      ): F[
        Option[
          (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[Par, ListParWithRandom]])
        ]
      ] =
        for {
          _ <- charge[F](
                storageCostConsume(channels, patterns, continuation).copy(
                  operation = "consume storage"
                )
              )
          consRes <- space.consume(
                      channels,
                      patterns,
                      continuation,
                      persist,
                      peeks
                    )
          id <- consumeId(continuation)
          _  <- handleResult(consRes, Consume(id, persist, channels.size))
        } yield consRes

      override def install(
          channels: Seq[Par],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation
      ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] =
        space.install(channels, patterns, continuation)

      override def produce(
          channel: Par,
          data: ListParWithRandom,
          persist: Boolean
      ): F[
        Option[
          (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[Par, ListParWithRandom]])
        ]
      ] =
        for {
          _       <- charge[F](storageCostProduce(channel, data).copy(operation = "produces storage"))
          prodRes <- space.produce(channel, data, persist)
          _       <- handleResult(prodRes, Produce(data.randomState, persist))
        } yield prodRes

      private def handleResult(
          result: Option[
            (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[Par, ListParWithRandom]])
          ],
          triggeredBy: TriggeredBy
      ): F[Unit] =
        result match {

          case None => charge[F](eventStorageCost(triggeredBy.channelsCount))

          case Some((cont, dataList)) =>
            for {
              consumeId <- consumeId(cont.continuation)

              // We refund for non-persistent continuations, and for the persistent continuation triggering the comm.
              // That persistent continuation is going to be charged for (without refund) once it has no matches in TS.
              refundForConsume = if (!cont.persistent || consumeId == triggeredBy.id) {
                storageCostConsume(cont.channels, cont.patterns, cont.continuation)
              } else {
                Cost(0)
              }
              refundForProduces = refundForRemovingProduces(dataList, cont, triggeredBy)

              _             <- charge[F](Cost(-refundForConsume.value, "consume storage refund"))
              _             <- charge[F](Cost(-refundForProduces.value, "produces storage refund"))
              lastIteration = !triggeredBy.persistent
              _             <- charge[F](eventStorageCost(triggeredBy.channelsCount)).whenA(lastIteration)
              _             <- charge[F](commEventStorageCost(cont.channels.size))
            } yield ()
        }

      private def refundForRemovingProduces(
          dataList: Seq[Result[Par, ListParWithRandom]],
          cont: ContResult[Par, BindPattern, TaggedContinuation],
          triggeredBy: TriggeredBy
      ): Cost = {
        val removedData = dataList
          .zip(cont.channels)
          // A persistent produce is charged for upfront before reaching the TS, and needs to be refunded
          // after each iteration it matches an existing consume. We treat it as 'removed' on each such iteration.
          // It is going to be 'not removed' and charged for on the last iteration, where it doesn't match anything.
          .filter {
            case (data, _) =>
              !data.persistent || data.removedDatum.randomState == triggeredBy.id
          }
        removedData
          .map { case (data, channel) => storageCostProduce(channel, data.removedDatum) }
          .foldLeft(Cost(0))(_ + _)
      }

    }
}
