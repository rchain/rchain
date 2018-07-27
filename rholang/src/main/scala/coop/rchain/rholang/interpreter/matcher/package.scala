package coop.rchain.rholang.interpreter

import cats.data.{OptionT, State, StateT}
import cats.implicits._
import cats.{Applicative, Functor, Monad}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.CostAccount

import scala.collection.immutable.Stream

package object matcher {

  type FreeMap            = Map[Int, Par]
  type OptionalFreeMap[A] = StateT[Option, FreeMap, A]

  type OptionalFreeMapWithCost[A] = StateT[OptionT[State[CostAccount, ?], ?], FreeMap, A]

  object OptionalFreeMapWithCost {

    class OptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) {
      def modifyCost(f: CostAccount => CostAccount): OptionalFreeMapWithCost[A] =
        StateT((m: FreeMap) => {
          OptionT(State((c: CostAccount) => {
            val (cost, result) = s.run(m).value.run(c).value
            (f(cost), result)
          }))
        })

      def runWithCost: (CostAccount, Option[(FreeMap, A)]) =
        s.run(Map.empty).value.run(CostAccount.zero).value
    }

    implicit def toOptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) =
      new OptionalFreeMapWithCostOps[A](s)

    def apply[A](
        f: FreeMap => OptionT[State[CostAccount, ?], (FreeMap, A)]): OptionalFreeMapWithCost[A] =
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
  }

  type NonDetFreeMapWithCost[A] = StateT[StreamT[State[CostAccount, ?], ?], FreeMap, A]

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
    }

    implicit def toNonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) =
      new NonDetFreeMapWithCostOps[A](s)

    def apply[A](
        f: FreeMap => StreamT[State[CostAccount, ?], (FreeMap, A)]): NonDetFreeMapWithCost[A] =
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

  type NonDetFreeMap[A] = StateT[Stream, FreeMap, A]

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
