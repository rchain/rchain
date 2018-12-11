package coop.rchain.rholang.interpreter.storage

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.{RhoISpace, RhoPureSpace}
import coop.rchain.rholang.interpreter.accounting.{CostAccounting, _}
import coop.rchain.rholang.interpreter.errors
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rspace.util._
import coop.rchain.rspace.{Blake2b256Hash, Checkpoint, ContResult, Result}

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

  def pureRSpace[F[_]: Sync](implicit costAlg: CostAccounting[F], space: RhoISpace[F]) =
    new RhoPureSpace[F] {

      override def consume(
          channels: Seq[Par],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation,
          persist: Boolean,
          sequenceNumber: Int
      ): F[Either[errors.OutOfPhlogistonsError.type, Option[
        (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[ListParWithRandomAndPhlos]])
      ]]] = {
        val storageCost = storageCostConsume(channels, patterns, continuation)
        for {
          _      <- costAlg.charge(storageCost)
          matchF <- costAlg.get().map(ca => matchListPar(ca.cost))
          consRes <- space.consume(channels, patterns, continuation, persist, sequenceNumber)(
                      matchF
                    )
          _ <- handleResult(consRes)
        } yield consRes
      }

      override def install(
          channels: Seq[Par],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation
      ): F[Option[(TaggedContinuation, Seq[ListParWithRandomAndPhlos])]] =
        space
          .install(channels, patterns, continuation)(
            matchListPar(Cost(Integer.MAX_VALUE))
          )

      override def produce(
          channel: Par,
          data: ListParWithRandom,
          persist: Boolean,
          sequenceNumber: Int
      ): F[Either[errors.OutOfPhlogistonsError.type, Option[
        (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[ListParWithRandomAndPhlos]])
      ]]] = {
        val storageCost = storageCostProduce(channel, data)
        for {
          _       <- costAlg.charge(storageCost)
          matchF  <- costAlg.get().map(ca => matchListPar(ca.cost))
          prodRes <- space.produce(channel, data, persist, sequenceNumber)(matchF)
          _       <- handleResult(prodRes)
        } yield prodRes
      }

      private def handleResult(
          result: Either[OutOfPhlogistonsError.type, Option[
            (
                ContResult[Par, BindPattern, TaggedContinuation],
                Seq[Result[ListParWithRandomAndPhlos]]
            )
          ]]
      ): F[Unit] =
        result match {
          case Left(oope) =>
            // if we run out of phlos during the match we have to zero phlos available
            costAlg.get().flatMap(ca => costAlg.charge(ca.cost)) >> Sync[F].raiseError(oope)

          case Right(None) => Sync[F].unit

          case Right(Some((cont, dataList))) =>
            val rspaceMatchCost = Cost(
              dataList
                .map(_.value.cost)
                .toList
                .sum
            )

            val refundForConsume =
              if (cont.persistent) Cost(0)
              else
                storageCostConsume(cont.channels, cont.patterns, cont.value)

            val refundForProduces = refundForRemovingProduces(
              dataList,
              cont.channels
            )

            for {
              _           <- costAlg.charge(rspaceMatchCost)
              refundValue = refundForConsume + refundForProduces
              _ <- if (refundValue == Cost(0))
                    Sync[F].unit
                  else costAlg.refund(refundValue)
            } yield ()
        }

      private def refundForRemovingProduces(
          dataList: Seq[Result[ListParWithRandomAndPhlos]],
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
