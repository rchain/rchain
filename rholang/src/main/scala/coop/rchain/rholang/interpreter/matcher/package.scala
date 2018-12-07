package coop.rchain.rholang.interpreter

import cats.arrow.FunctionK
import cats.data.{OptionT, StateT}
import cats.implicits._
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

import scala.collection.immutable.Stream

package object matcher {

  type FreeMap = Map[Int, Par]

  //FreeMap => Cost => Either[OOPE, (Cost, Option[(FreeMap, A)])]
  type OptionalFreeMapWithCost[A] = StateT[OptionWithCost, FreeMap, A]
  type OptionWithCost[A]          = OptionT[ErroredOrCostA, A]

  type OOPE[A]           = Either[OutOfPhlogistonsError.type, A]
  type ErroredOrCostA[A] = StateT[OOPE, Cost, A]

  //FreeMap => Cost => Either[OOPE, (Cost, Stream[(FreeMap, A)])]
  type NonDetFreeMapWithCost[A] = StateT[StreamWithCost, FreeMap, A]
  type StreamWithCost[A]        = StreamT[ErroredOrCostA, A]

  object OptionalFreeMapWithCost {

    class OptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) {
      def charge(amount: Cost): OptionalFreeMapWithCost[A] =
        StateT((m: FreeMap) => {
          OptionT(StateT((c: Cost) => {
            s.run(m).value.run(c).flatMap {
              case (cost, result) =>
                val newCost = cost - amount
                if (newCost.value < 0)
                  Left(OutOfPhlogistonsError)
                else
                  Right((newCost, result))
            }
          }))
        })

      def attemptOpt: OptionalFreeMapWithCost[Option[A]] =
        StateT((m: FreeMap) => {
          OptionT(StateT((c: Cost) => {
            s.run(m).value.run(c).map {
              case (cost, result) =>
                val recovered: Option[(FreeMap, Option[A])] = result match {
                  case None          => Some((m, None))
                  case Some((m1, a)) => Some((m1, Some(a)))
                }

                (cost, recovered)
            }
          }))
        })

      def runWithCost(
          initCost: Cost
      ): Either[OutOfPhlogistonsError.type, (Cost, Option[(FreeMap, A)])] =
        s.run(Map.empty).value.run(initCost)

      def toNonDet(): NonDetFreeMapWithCost[A] =
        s.mapK[StreamWithCost](new FunctionK[OptionWithCost, StreamWithCost] {
          override def apply[T](fa: OptionWithCost[T]): StreamWithCost[T] =
            StreamT.fromStream(fa.fold(Stream.empty[T])(single => Stream(single)))
        })
    }

    implicit def toOptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) =
      new OptionalFreeMapWithCostOps[A](s)

    def apply[A](f: FreeMap => OptionWithCost[(FreeMap, A)]): OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        f(m)
      })

    def empty[A]: OptionalFreeMapWithCost[A] =
      StateT.liftF(OptionT.fromOption(None))

    def pure[A](value: A): OptionalFreeMapWithCost[A] =
      StateT.pure[OptionWithCost, FreeMap, A](value)

    def fromOption[A](option: Option[A]): OptionalFreeMapWithCost[A] =
      StateT.liftF(OptionT.fromOption(option))
  }

  object NonDetFreeMapWithCost {
    class NonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) {
      def charge(amount: Cost): NonDetFreeMapWithCost[A] =
        StateT((m: FreeMap) => {
          StreamT(StateT((c: Cost) => {
            s.run(m).next.run(c).flatMap {
              case (cost, result) =>
                val newCost = cost - amount
                if (newCost.value < 0)
                  Left(OutOfPhlogistonsError)
                else
                  Right((newCost, result))
            }
          }))
        })

      def runWithCost(
          initCost: Cost
      ): Either[OutOfPhlogistonsError.type, (Cost, Stream[(FreeMap, A)])] =
        StreamT.run(s.run(Map.empty)).run(initCost)

      def toDet(): OptionalFreeMapWithCost[A] =
        s.mapK[OptionWithCost](new FunctionK[StreamWithCost, OptionWithCost] {
          override def apply[T](fa: StreamWithCost[T]): OptionWithCost[T] =
            OptionT(StreamT.run(fa).map(_.headOption))
        })
    }

    implicit def toNonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) =
      new NonDetFreeMapWithCostOps[A](s)

    def empty[A]: NonDetFreeMapWithCost[A] =
      StateT.liftF(StreamT.empty)

    def pure[A](value: A): NonDetFreeMapWithCost[A] =
      StateT.pure[StreamWithCost, FreeMap, A](value)

    def fromStream[A](stream: Stream[A]): NonDetFreeMapWithCost[A] =
      StateT.liftF(StreamT.fromStream(StateT.pure(stream)))
  }

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
