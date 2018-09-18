package coop.rchain.rholang.interpreter

import cats.arrow.FunctionK
import cats.data.{OptionT, State, StateT}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.CostAccount

import scala.collection.immutable.Stream

package object matcher {

  type FreeMap = Map[Int, Par]

  type OptionalFreeMapWithCost[A] = StateT[OptionWithCost, FreeMap, A]
  type OptionWithCost[A]          = OptionT[State[CostAccount, ?], A]

  object OptionalFreeMapWithCost {

    class OptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) {
      def modifyCost(f: CostAccount => CostAccount): OptionalFreeMapWithCost[A] =
        StateT((m: FreeMap) => {
          OptionT(State((c: CostAccount) => {
            val (cost, result) = s.run(m).value.run(c).value
            (f(cost), result)
          }))
        })

      def attemptOpt: OptionalFreeMapWithCost[Option[A]] =
        StateT((m: FreeMap) => {
          OptionT(State((c: CostAccount) => {
            val (cost: CostAccount, result: Option[(FreeMap, A)]) = s.run(m).value.run(c).value

            val recovered: Option[(FreeMap, Option[A])] = result match {
              case None          => Some((m, None))
              case Some((m1, a)) => Some((m1, Some(a)))
            }

            (cost, recovered)
          }))
        })

      def runWithCost: (CostAccount, Option[(FreeMap, A)]) =
        s.run(Map.empty).value.run(CostAccount.zero).value

      def toNonDet(): NonDetFreeMapWithCost[A] =
        s.mapK[StreamWithCost](new FunctionK[OptionWithCost, StreamWithCost] {
          override def apply[T](fa: OptionWithCost[T]): StreamWithCost[T] =
            StreamT(fa.fold(Stream.empty[T])(single => Stream(single)))
        })
    }

    implicit def toOptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) =
      new OptionalFreeMapWithCostOps[A](s)

    def apply[A](f: FreeMap => OptionWithCost[(FreeMap, A)]): OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        f(m)
      })

    def emptyMap[A]: OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        OptionT(
          State((c: CostAccount) => {
            (c, None)
          })
        )
      })

    def pure[A](value: A): OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        OptionT(
          State((c: CostAccount) => {
            (c, Some((m, value)))
          })
        )
      })

    def liftF[A](option: Option[A]): OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        OptionT(State((c: CostAccount) => {
          (c, option.map(m -> _))
        }))
      })
  }

  type NonDetFreeMapWithCost[A] = StateT[StreamWithCost, FreeMap, A]
  type StreamWithCost[A]        = StreamT[State[CostAccount, ?], A]

  object NonDetFreeMapWithCost {
    class NonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) {
      def modifyCost(f: CostAccount => CostAccount): NonDetFreeMapWithCost[A] =
        StateT((m: FreeMap) => {
          StreamT(State((c: CostAccount) => {
            val (cost, result) = s.run(m).value.run(c).value
            (f(cost), result)
          }))
        })

      def runWithCost: (CostAccount, Stream[(FreeMap, A)]) =
        s.run(Map.empty).value.run(CostAccount.zero).value

      def toDet(): OptionalFreeMapWithCost[A] =
        s.mapK[OptionWithCost](new FunctionK[StreamWithCost, OptionWithCost] {
          override def apply[T](fa: StreamWithCost[T]): OptionWithCost[T] =
            OptionT(fa.value.map(_.headOption))
        })
    }

    implicit def toNonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) =
      new NonDetFreeMapWithCostOps[A](s)

    def apply[A](f: FreeMap => StreamWithCost[(FreeMap, A)]): NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => f(m))

    def emptyMap[A]: NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        StreamT(State((c: CostAccount) => {
          (c, Stream.empty)
        }))
      })

    def pure[A](value: A): NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        StreamT(State((c: CostAccount) => {
          (c, Stream((m, value)))
        }))
      })

    def liftF[A](stream: Stream[A]): NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        StreamT(State((c: CostAccount) => {
          (c, stream.map(m -> _))
        }))
      })

  }

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
