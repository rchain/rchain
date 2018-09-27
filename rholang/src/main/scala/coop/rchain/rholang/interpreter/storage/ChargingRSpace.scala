package coop.rchain.rholang.interpreter.storage

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.{RhoISpace, RhoPureSpace}
import coop.rchain.rholang.interpreter.accounting.CostAccount._
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg, _}
import coop.rchain.rholang.interpreter.errors
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.rspace.{Blake2b256Hash, Checkpoint}

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

  def pureRSpace[F[_]: Sync](implicit costAlg: CostAccountingAlg[F], space: RhoISpace) =
    new RhoPureSpace[F] {

      override def consume(
          channels: Seq[Par],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation,
          persist: Boolean
      ): F[Either[errors.OutOfPhlogistonsError.type, Option[
        (TaggedContinuation, Seq[ListParWithRandom])
      ]]] = {
        val storageCost = storageCostConsume(channels, patterns, continuation)
        for {
          _       <- costAlg.charge(storageCost)
          matchF  <- costAlg.get().map(ca => matchListPar(ca.cost))
          consRes <- Sync[F].delay(space.consume(channels, patterns, continuation, persist)(matchF))
          _       <- handleResult(consRes, storageCost, persist)
        } yield consRes
      }

      override def install(
          channels: Seq[Par],
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation
      ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] =
        Sync[F].delay(
          space.install(channels, patterns, continuation)(
            matchListPar(Cost(Integer.MAX_VALUE))
          )
        )

      override def produce(
          channel: Par,
          data: ListParWithRandom,
          persist: Boolean
      ): F[Either[errors.OutOfPhlogistonsError.type, Option[
        (TaggedContinuation, Seq[ListParWithRandom])
      ]]] = {
        val storageCost = storageCostProduce(channel, data)
        for {
          _       <- costAlg.charge(storageCost)
          matchF  <- costAlg.get().map(ca => matchListPar(ca.cost))
          prodRes <- Sync[F].delay(space.produce(channel, data, persist)(matchF))
          _       <- handleResult(prodRes, storageCost, persist)
        } yield prodRes
      }

      private def handleResult(
          result: Either[OutOfPhlogistonsError.type, Option[
            (TaggedContinuation, Seq[ListParWithRandom])
          ]],
          storageCost: Cost,
          persist: Boolean
      ): F[Unit] =
        result match {
          case Left(oope) =>
            // if we run out of phlos during the match we have to zero phlos available
            costAlg.get().flatMap(ca => costAlg.charge(ca.cost)) >> Sync[F].raiseError(oope)
          case Right(Some((_, dataList))) =>
            val rspaceMatchCost = Cost(
              dataList
                .map(_.cost)
                .toList
                .combineAll
            )

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
