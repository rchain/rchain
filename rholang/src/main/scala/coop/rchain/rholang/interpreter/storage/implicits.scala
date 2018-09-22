package coop.rchain.rholang.interpreter.storage

import java.util.concurrent.atomic.AtomicReference

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.rholang.interpreter.accounting.CostAccount
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.matcher.OptionalFreeMapWithCost._
import coop.rchain.rholang.interpreter.matcher._
import coop.rchain.rspace.{Serialize, Match => StorageMatch}

//noinspection ConvertExpressionToSAM
object implicits {

  /* Match instance */

  private def toChannels(fm: FreeMap, max: Int): Seq[Channel] =
    (0 until max).map { (i: Int) =>
      fm.get(i) match {
        case Some(par) => Channel(Quote(par))
        case None      => Channel(Quote(Par.defaultInstance))
      }
    }

  def matchListQuote(init: CostAccount): StorageMatch[
    BindPattern,
    OutOfPhlogistonsError.type,
    ListChannelWithRandom,
    ListChannelWithRandom
  ] =
    new StorageMatch[
      BindPattern,
      OutOfPhlogistonsError.type,
      ListChannelWithRandom,
      ListChannelWithRandom
    ] {

      private val phlosState = new AtomicReference[CostAccount](init)

      private def calcUsed(init: CostAccount, left: CostAccount): CostAccount =
        CostAccount(left.idx - init.idx, init.cost - left.cost)

      def get(
          pattern: BindPattern,
          data: ListChannelWithRandom
      ): Either[OutOfPhlogistonsError.type, Option[ListChannelWithRandom]] = {
        val startPhlos = phlosState.get
        SpatialMatcher
          .foldMatch(data.channels, pattern.patterns, pattern.remainder)
          .runWithCost(startPhlos)
          .map {
            case (left, resultMatch) =>
              val cost = calcUsed(startPhlos, left)
              phlosState.getAndUpdate(_ - cost)
              resultMatch
                .map {
                  case (freeMap: FreeMap, caughtRem: Seq[Channel]) =>
                    val remainderMap = pattern.remainder match {
                      case Some(Var(FreeVar(level))) =>
                        val flatRem: Seq[Par] = caughtRem.collect {
                          case Channel(Quote(p)) => p
                        }
                        freeMap + (level -> VectorPar().addExprs(EList(flatRem.toVector)))
                      case _ => freeMap
                    }
                    ListChannelWithRandom(
                      toChannels(remainderMap, pattern.freeCount),
                      data.randomState,
                      Some(CostAccount.toProto(cost))
                    )
                }
          }
      }
    }

  /* Serialize instances */

  implicit val serializeBindPattern: Serialize[BindPattern] =
    mkProtobufInstance(BindPattern)

  implicit val serializeChannel: Serialize[Channel] =
    mkProtobufInstance(Channel)

  implicit val serializeChannels: Serialize[ListChannelWithRandom] =
    mkProtobufInstance(ListChannelWithRandom)

  implicit val serializeTaggedContinuation: Serialize[TaggedContinuation] =
    mkProtobufInstance(TaggedContinuation)
}
