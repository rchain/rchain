package coop.rchain.rholang.interpreter.storage
import cats.implicits._
import coop.rchain.models.{BindPattern, Channel, ListChannelWithRandom, TaggedContinuation}
import coop.rchain.rholang.interpreter.Runtime.RhoPureSpace
import coop.rchain.rholang.interpreter.accounting.CostAccount._
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
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
                      Option[(TaggedContinuation, Seq[ListChannelWithRandom])]]] =
        for {
          //TODO(mateusz.gorski): move charging for the storage from Reduce here
          //TODO(mateusz.gorski): refund the phlos if the match was found
          consRes <- space.consume(channels, patterns, continuation, persist)
          _       <- handleResult(consRes)
        } yield consRes

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
                      Option[(TaggedContinuation, Seq[ListChannelWithRandom])]]] =
        for {
          //TODO(mateusz.gorski): move charging for the storage from Reduce here
          //TODO(mateusz.gorski): refund the phlos if the match was found
          prodRes <- space.produce(channel, data, persist)
          _       <- handleResult(prodRes)
        } yield prodRes

      private def handleResult(
          result: Either[OutOfPhlogistonsError.type,
                         Option[(TaggedContinuation, Seq[ListChannelWithRandom])]]): Task[Unit] =
        result match {
          case Left(oope) =>
            // if we run out of phlos during the match we have to zero phlos available
            costAlg.get().flatMap(costAlg.charge(_)) >> Task.raiseError(oope)
          case Right(Some((_, dataList))) =>
            val rspaceMatchCost = dataList
              .map(_.cost.map(CostAccount.fromProto(_)).getOrElse(CostAccount(0)))
              .toList
              .combineAll
            //                  .map(ca => ca.copy(cost = Cost(Integer.MAX_VALUE) - ca.cost))
            costAlg.charge(rspaceMatchCost)
          case Right(None) =>
            Task.unit
        }

      override def createCheckpoint(): Task[Checkpoint] =
        space.createCheckpoint()
      override def reset(hash: Blake2b256Hash): Task[Unit] = space.reset(hash)
      override def close(): Task[Unit]                     = space.close()
    }
}
