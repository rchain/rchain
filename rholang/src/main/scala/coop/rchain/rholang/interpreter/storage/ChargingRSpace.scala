package coop.rchain.rholang.interpreter.storage

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.CostAccount._
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg, _}
import coop.rchain.rholang.interpreter.errors
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.rspace.{Blake2b256Hash, Checkpoint}

import scala.collection.immutable.Seq

object ChargingRSpace {
  def storageCostConsume(
      channels: Seq[Channel],
      patterns: Seq[BindPattern],
      continuation: TaggedContinuation
  ): Cost = {
    val bodyCost = Some(continuation).collect {
      case TaggedContinuation(ParBody(ParWithRandom(body, _))) => body.storageCost
    }
    channels.storageCost + patterns.storageCost + bodyCost.getOrElse(Cost(0))
  }

  def storageCostProduce(channel: Channel, data: ListChannelWithRandom): Cost =
    channel.storageCost + data.channels.storageCost

  def pureRSpace[F[_]: Sync](implicit costAlg: CostAccountingAlg[F], space: RhoISpace) =
    new PureRSpace[
      F,
      Channel,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListChannelWithRandom,
      ListChannelWithRandom,
      TaggedContinuation
    ] {

      override def consume(
          channels: Seq[Channel],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation,
          persist: Boolean
      ): F[Either[errors.OutOfPhlogistonsError.type, Option[
        (TaggedContinuation, Seq[ListChannelWithRandom])
      ]]] = {
        val storageCost = storageCostConsume(channels, patterns, continuation)
        for {
          _       <- costAlg.charge(storageCost)
          consRes <- Sync[F].delay(space.consume(channels, patterns, continuation, persist))
          _       <- handleResult(consRes, storageCost, persist)
        } yield consRes
      }

      override def install(
          channels: Seq[Channel],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation
      ): F[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
        Sync[F].delay(space.install(channels, patterns, continuation))

      override def produce(
          channel: Channel,
          data: ListChannelWithRandom,
          persist: Boolean
      ): F[Either[errors.OutOfPhlogistonsError.type, Option[
        (TaggedContinuation, Seq[ListChannelWithRandom])
      ]]] = {
        val storageCost = storageCostProduce(channel, data)
        for {
          _       <- costAlg.charge(storageCost)
          prodRes <- Sync[F].delay(space.produce(channel, data, persist))
          _       <- handleResult(prodRes, storageCost, persist)
        } yield prodRes
      }

      private def handleResult(
          result: Either[OutOfPhlogistonsError.type, Option[
            (TaggedContinuation, Seq[ListChannelWithRandom])
          ]],
          storageCost: Cost,
          persist: Boolean
      ): F[Unit] =
        result match {
          case Left(oope) =>
            // if we run out of phlos during the match we have to zero phlos available
            costAlg.get().flatMap(costAlg.charge(_)) >> Sync[F].raiseError(oope)
          case Right(Some((_, dataList))) =>
            val rspaceMatchCost = dataList
              .map(_.cost.map(CostAccount.fromProto(_)).get)
              .toList
              .combineAll

            costAlg
              .charge(rspaceMatchCost)
              .flatMap { _ =>
                // we refund the storage cost if there was a match and the persist flag is false
                // this means that the data didn't stay in the tuplespace
                if (persist)
                  Sync[F].unit
                else {
                  costAlg.refund(storageCost)
                }
              }
          case Right(None) =>
            Sync[F].unit
        }

      override def createCheckpoint(): F[Checkpoint] =
        Sync[F].delay(space.createCheckpoint())
      override def reset(hash: Blake2b256Hash): F[Unit] = Sync[F].delay(space.reset(hash))
      override def close(): F[Unit]                     = Sync[F].delay(space.close())
    }
}
