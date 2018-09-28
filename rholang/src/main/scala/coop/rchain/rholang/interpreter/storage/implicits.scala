package coop.rchain.rholang.interpreter.storage

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.rholang.interpreter.accounting.Cost
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

  def matchListQuote(init: Cost): StorageMatch[
    BindPattern,
    OutOfPhlogistonsError.type,
    ListChannelWithRandom,
    ListChannelWithRandomAndPhlos
  ] =
    new StorageMatch[
      BindPattern,
      OutOfPhlogistonsError.type,
      ListChannelWithRandom,
      ListChannelWithRandomAndPhlos
    ] {

      private def calcUsed(init: Cost, left: Cost): Cost = init - left

      def get(
          pattern: BindPattern,
          data: ListChannelWithRandom
      ): Either[OutOfPhlogistonsError.type, Option[ListChannelWithRandomAndPhlos]] =
        SpatialMatcher
          .foldMatch(data.channels, pattern.patterns, pattern.remainder)
          .runWithCost(init)
          .map {
            case (left, resultMatch) =>
              val cost = calcUsed(init, left)
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
                    ListChannelWithRandomAndPhlos(
                      toChannels(remainderMap, pattern.freeCount),
                      data.randomState,
                      cost.value
                    )
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
