package coop.rchain.rholang.interpreter.storage

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

  private def toSeq(fm: FreeMap, max: Int): Seq[Par] =
    (0 until max).map { (i: Int) =>
      fm.get(i) match {
        case Some(par) => par
        case None      => Par.defaultInstance
      }
    }

  def matchListPar(init: Cost): StorageMatch[
    BindPattern,
    OutOfPhlogistonsError.type,
    ListParWithRandom,
    ListParWithRandomAndPhlos
  ] =
    new StorageMatch[
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos
    ] {

      private def calcUsed(init: Cost, left: Cost): Cost = init - left

      def get(
          pattern: BindPattern,
          data: ListParWithRandom
      ): Either[OutOfPhlogistonsError.type, Option[ListParWithRandomAndPhlos]] =
        SpatialMatcher
          .foldMatch(data.pars, pattern.patterns, pattern.remainder)
          .runWithCost(init)
          .map {
            case (left, resultMatch) =>
              val cost = calcUsed(init, left)
              resultMatch
                .map {
                  case (freeMap: FreeMap, caughtRem: Seq[Par]) =>
                    val remainderMap = pattern.remainder match {
                      case Some(Var(FreeVar(level))) =>
                        freeMap + (level -> VectorPar().addExprs(EList(caughtRem.toVector)))
                      case _ => freeMap
                    }
                    ListParWithRandomAndPhlos(
                      toSeq(remainderMap, pattern.freeCount),
                      data.randomState,
                      cost.value
                    )
                }
          }
    }

  /* Serialize instances */

  implicit val serializeBindPattern: Serialize[BindPattern] =
    mkProtobufInstance(BindPattern)

  implicit val serializePar: Serialize[Par] =
    mkProtobufInstance(Par)

  implicit val serializePars: Serialize[ListParWithRandom] =
    mkProtobufInstance(ListParWithRandom)

  implicit val serializeTaggedContinuation: Serialize[TaggedContinuation] =
    mkProtobufInstance(TaggedContinuation)
}
