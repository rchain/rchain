package coop.rchain.rholang.interpreter.storage

import cats.effect.Sync
import cats.implicits._
import cats.mtl.implicits._
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.rholang.interpreter.accounting._
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

  def matchListPar[F[_]: Sync](
      implicit
      cost: _cost[F]
  ): StorageMatch[F, BindPattern, ListParWithRandom, ListParWithRandomAndPhlos] =
    new StorageMatch[F, BindPattern, ListParWithRandom, ListParWithRandomAndPhlos] {
      def get(
          pattern: BindPattern,
          data: ListParWithRandom
      ): F[Option[ListParWithRandomAndPhlos]] = {
        type R[A] = MatcherMonadT[F, A]
        implicit val _ = matcherMonadCostLog[F]()
        for {
          init <- cost.get
          matchResult <- runFirst[F, Seq[Par]](
                          SpatialMatcher
                            .foldMatch[R, Par, Par](
                              data.pars,
                              pattern.patterns,
                              pattern.remainder
                            )
                        )
          left <- cost.get
        } yield {
          matchResult.map {
            case (freeMap, caughtRem) =>
              val remainderMap = pattern.remainder match {
                case Some(Var(FreeVar(level))) =>
                  freeMap + (level -> VectorPar().addExprs(EList(caughtRem.toVector)))
                case _ => freeMap
              }
              ListParWithRandomAndPhlos(
                toSeq(remainderMap, pattern.freeCount),
                data.randomState,
                (init - left).value
              )
          }
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
