package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import cats.mtl.implicits._
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.matcher._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.{Channel, Match => StorageMatch}
import coop.rchain.shared.Serialize

//noinspection ConvertExpressionToSAM
package object storage {

  /* Match instance */

  private def toSeq(fm: FreeMap, max: Int): Vector[Par] =
    (0 until max).map { (i: Int) =>
      fm.get(i) match {
        case Some(par) => par
        case None      => Par.defaultInstance
      }
    }.toVector

  def matchListPar[F[_]: Sync: Span]: StorageMatch[F, BindPattern, ListParWithRandom] =
    new StorageMatch[F, BindPattern, ListParWithRandom] {
      def get(
          pattern: BindPattern,
          data: ListParWithRandom
      ): F[Option[ListParWithRandom]] = {
        type R[A] = MatcherMonadT[F, A]
        implicit val matcherMonadError = implicitly[Sync[R]]
        for {
          matchResult <- runFirst[F, Vector[Par]](
                          SpatialMatcher
                            .foldMatch[R, Par, Par](
                              data.pars,
                              pattern.patterns,
                              pattern.remainder
                            )
                        )
        } yield {
          matchResult.map {
            case (freeMap, caughtRem) =>
              val remainderMap = pattern.remainder match {
                case Some(Var(FreeVar(level))) =>
                  freeMap + (level -> VectorPar().addExprs(EList(caughtRem)))
                case _ => freeMap
              }
              ListParWithRandom(
                toSeq(remainderMap, pattern.freeCount),
                data.randomState
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

  def parToChannel(par: Par): Channel = {
    val bytes = storage.serializePar.encode(par)
    val hash  = Blake2b256Hash.create(bytes.toArray)
    Channel(hash)
  }
}
