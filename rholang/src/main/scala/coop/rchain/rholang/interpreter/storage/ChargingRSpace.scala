package coop.rchain.rholang.interpreter.storage

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.{RhoISpace, RhoPureSpace}
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter._error
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rspace.util._
import coop.rchain.rspace.{Blake2b256Hash, Checkpoint, ContResult, Result, Match => StorageMatch}

import scala.collection.immutable.Seq

object ChargingRSpace {
  def storageCostConsume(
      channels: Seq[Par],
      patterns: Seq[BindPattern],
      continuation: TaggedContinuation
  ): Cost = {
    val bodyCost = Some(continuation).collect {
      case TaggedContinuation(ParBody(ParWithRandom(body, _))) => body.storageCost
    }
    channels.storageCost + patterns.storageCost + bodyCost.getOrElse(Cost(0))
  }

  def storageCostProduce(channel: Par, data: ListParWithRandom): Cost =
    channel.storageCost + data.pars.storageCost

  def pureRSpace[F[_]: Sync](
      space: RhoISpace[F]
  )(implicit cost: _cost[F]) =
    new RhoPureSpace[F] {

      implicit val m: StorageMatch[F, BindPattern, ListParWithRandom, ListParWithRandom] =
        matchListPar[F]

      override def consume(
          channels: Seq[Par],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation,
          persist: Boolean,
          sequenceNumber: Int
      ): F[
        Option[(ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[ListParWithRandom]])]
      ] =
        for {
          _       <- charge[F](storageCostConsume(channels, patterns, continuation))
          consRes <- space.consume(channels, patterns, continuation, persist, sequenceNumber)
          _       <- handleResult(consRes)
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
          persist: Boolean,
          sequenceNumber: Int
      ): F[
        Option[(ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[ListParWithRandom]])]
      ] =
        for {
          _       <- charge[F](storageCostProduce(channel, data))
          prodRes <- space.produce(channel, data, persist, sequenceNumber)
          _       <- handleResult(prodRes)
        } yield prodRes

      private def handleResult(
          result: Option[
            (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[ListParWithRandom]])
          ]
      ): F[Unit] =
        result match {

          case None => Sync[F].unit

          case Some((cont, dataList)) =>
            val refundForConsume =
              if (cont.persistent) Cost(0)
              else
                storageCostConsume(cont.channels, cont.patterns, cont.value)

            val refundForProduces = refundForRemovingProduces(
              dataList,
              cont.channels
            )

            val refundValue = refundForConsume + refundForProduces

            for {
              _ <- if (refundValue == Cost(0))
                    Sync[F].unit
                  else charge[F](Cost(-refundValue.value, "storage refund"))
            } yield ()
        }

      private def refundForRemovingProduces(
          dataList: Seq[Result[ListParWithRandom]],
          channels: Seq[Par]
      ): Cost =
        dataList
          .zip(channels)
          .filterNot { case (data, _) => data.persistent }
          .map {
            case (data, channel) =>
              storageCostProduce(channel, ListParWithRandom(data.pars, data.randomState))
          }
          .foldLeft(Cost(0))(_ + _)

      override def createCheckpoint(): F[Checkpoint]    = space.createCheckpoint()
      override def reset(hash: Blake2b256Hash): F[Unit] = space.reset(hash)
      override def close(): F[Unit]                     = space.close()
    }
}
