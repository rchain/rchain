package coop.rchain.rholang.interpreter.storage
import cats.implicits._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.RhoPureSpace
import coop.rchain.rholang.interpreter.accounting.CostAccount._
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg, _}
import coop.rchain.rholang.interpreter.errors
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.rspace.{Blake2b256Hash, Checkpoint, Match}
import monix.eval.Task

import scala.collection.immutable.Seq

object ChargingRSpace {
  def pureRSpace(implicit costAlg: CostAccountingAlg[Task], space: RhoPureSpace) =
    new PureRSpace[Task,
                   Channel,
                   BindPattern,
                   OutOfPhlogistonsError.type,
                   ListChannelWithRandom,
                   ListChannelWithRandom,
                   TaggedContinuation] {

      override def consume(channels: Seq[Channel],
                           patterns: Seq[BindPattern],
                           continuation: TaggedContinuation,
                           persist: Boolean)(implicit m: Match[BindPattern,
                                                               errors.OutOfPhlogistonsError.type,
                                                               ListChannelWithRandom,
                                                               ListChannelWithRandom])
        : Task[Either[errors.OutOfPhlogistonsError.type,
                      Option[(TaggedContinuation, Seq[ListChannelWithRandom])]]] = {
        val bodyCost = Some(continuation).collect {
          case TaggedContinuation(ParBody(ParWithRandom(body, _))) => body.storageCost
        }
        val storageCost = channels.storageCost + patterns.storageCost + bodyCost.getOrElse(Cost(0))
        for {
          _       <- costAlg.charge(storageCost)
          consRes <- space.consume(channels, patterns, continuation, persist)
          _       <- handleResult(consRes, storageCost, persist)
        } yield consRes
      }

      override def install(channels: Seq[Channel],
                           patterns: Seq[BindPattern],
                           continuation: TaggedContinuation)(
          implicit m: Match[BindPattern,
                            errors.OutOfPhlogistonsError.type,
                            ListChannelWithRandom,
                            ListChannelWithRandom])
        : Task[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
        space.install(channels, patterns, continuation) // install is free

      override def produce(channel: Channel, data: ListChannelWithRandom, persist: Boolean)(
          implicit m: Match[BindPattern,
                            errors.OutOfPhlogistonsError.type,
                            ListChannelWithRandom,
                            ListChannelWithRandom])
        : Task[Either[errors.OutOfPhlogistonsError.type,
                      Option[(TaggedContinuation, Seq[ListChannelWithRandom])]]] = {
        val storageCost = channel.storageCost + data.channels.storageCost
        for {
          prodRes <- space.produce(channel, data, persist)
          _       <- handleResult(prodRes, storageCost, persist)
        } yield prodRes
      }

      private def handleResult(
          result: Either[OutOfPhlogistonsError.type,
                         Option[(TaggedContinuation, Seq[ListChannelWithRandom])]],
          storageCost: Cost,
          persist: Boolean): Task[Unit] =
        result match {
          case Left(oope) =>
            // if we run out of phlos during the match we have to zero phlos available
            costAlg.get().flatMap(costAlg.charge(_)) >> Task.raiseError(oope)
          case Right(Some((_, dataList))) =>
            val rspaceMatchCost = dataList
              .map(_.cost.map(CostAccount.fromProto(_)).getOrElse(CostAccount(0)))
              .toList
              .map(ca => ca.copy(cost = Cost(Integer.MAX_VALUE) - ca.cost)) //FIXME: This shouldn't be needed
              .combineAll

            costAlg
              .charge(rspaceMatchCost)
              .flatMap { _ =>
                // we refund the storage cost if there was a match and the persist flag is boolean
                // this means that the data didn't stay in the tuplespace
                if (persist)
                  Task.unit
                else
                  costAlg.refund(storageCost)
              }
          case Right(None) =>
            Task.unit
        }

      override def createCheckpoint(): Task[Checkpoint] =
        space.createCheckpoint()
      override def reset(hash: Blake2b256Hash): Task[Unit] = space.reset(hash)
      override def close(): Task[Unit]                     = space.close()
    }
}
