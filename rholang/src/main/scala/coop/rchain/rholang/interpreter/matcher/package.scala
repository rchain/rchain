package coop.rchain.rholang.interpreter

import cats.arrow.FunctionK
import cats.data.{OptionT, State, StateT}
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting.CostAccount
import cats.implicits._
import cats.data._
import coop.rchain.rholang.interpreter.errors.OutOfPhloError
import monix.eval.Coeval

import scala.collection.immutable.Stream

package object matcher {

  type FreeMap = Map[Int, Par]

  type OptionalFreeMapWithCost[A] =
    StateT[OptionT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?], FreeMap, A]

  object OptionalFreeMapWithCost {

    class OptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) {
      def modifyCost(f: CostAccount => CostAccount): OptionalFreeMapWithCost[A] =
        StateT((m: FreeMap) => {
          OptionT(StateT((c: CostAccount) => {
            s.run(m).value.run(c).flatMap {
              case (cost, result) =>
                val newCost = f(cost)
                if (newCost.cost.value < 0)
                  Left(OutOfPhloError())
                else
                  Right((newCost, result))
            }
          }))
        })

      def attempt: OptionalFreeMapWithCost[Either[Unit, A]] =
        StateT((m: FreeMap) => {
          OptionT(StateT((c: CostAccount) => {
            s.run(m).value.run(c).map {
              case (cost: CostAccount, result: Option[(FreeMap, A)]) =>
                val recovered: Option[(FreeMap, Either[Unit, A])] = result match {
                  case None          => Some((m, Left(())))
                  case Some((m1, a)) => Some((m1, Right(a)))
                }
                (cost, recovered)
            }
          }))
        })

      def runWithCost(initCost: CostAccount = CostAccount.zero)
        : Either[OutOfPhloError, (CostAccount, Option[(FreeMap, A)])] =
        s.run(Map.empty).value.run(initCost)

      def toNonDet(): NonDetFreeMapWithCost[A] =
        s.mapK[StreamT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?]](
          new FunctionK[OptionT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?],
                        StreamT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?]] {
            override def apply[A](fa: OptionT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], A])
              : StreamT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], A] =
              StreamT(fa.fold(Stream.empty[A])(single => Stream(single)))
          })
    }

    implicit def toOptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) =
      new OptionalFreeMapWithCostOps[A](s)

    def apply[A](
        f: FreeMap => OptionT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], (FreeMap, A)])
      : OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        f(m)
      })

    def emptyMap[A]: OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        OptionT(
          StateT((c: CostAccount) => {
            Right((c, None))
          })
        )
      })

    def pure[A](value: A): OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        OptionT(
          StateT((c: CostAccount) => {
            Right((c, Some((m, value))))
          })
        )
      })

    def liftF[A](option: Option[A]): OptionalFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        OptionT(StateT((c: CostAccount) => {
          Right((c, option.map(m -> _)))
        }))
      })
  }

  type NonDetFreeMapWithCost[A] =
    StateT[StreamT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?], FreeMap, A]

  object NonDetFreeMapWithCost {
    class NonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) {
      def modifyCost(f: CostAccount => CostAccount): NonDetFreeMapWithCost[A] =
        StateT((m: FreeMap) => {
          StreamT(StateT((c: CostAccount) => {
            s.run(m).value.run(c).flatMap {
              case (cost, result) =>
                val newCost = f(cost)
                if (newCost.cost.value < 0)
                  Left(OutOfPhloError())
                else
                  Right((newCost, result))
            }
          }))
        })

      def runWithCost(initCost: CostAccount = CostAccount.zero)
        : Either[OutOfPhloError, (CostAccount, Stream[(FreeMap, A)])] =
        s.run(Map.empty).value.run(CostAccount.zero)

      def toDet(): OptionalFreeMapWithCost[A] =
        s.mapK[OptionT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?]](
          new FunctionK[StreamT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?],
                        OptionT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], ?]] {
            override def apply[A](fa: StreamT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], A])
              : OptionT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], A] =
              OptionT(fa.value.map(_.headOption))
          })
    }

    implicit def toNonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) =
      new NonDetFreeMapWithCostOps[A](s)

    def apply[A](
        f: FreeMap => StreamT[StateT[Either[OutOfPhloError, ?], CostAccount, ?], (FreeMap, A)])
      : NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => f(m))

    def emptyMap[A]: NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        StreamT(StateT((c: CostAccount) => {
          Right((c, Stream.empty))
        }))
      })

    def pure[A](value: A): NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        StreamT(StateT((c: CostAccount) => {
          Right((c, Stream((m, value))))
        }))
      })

    def liftF[A](stream: Stream[A]): NonDetFreeMapWithCost[A] =
      StateT((m: FreeMap) => {
        StreamT(StateT((c: CostAccount) => {
          Right((c, stream.map(m -> _)))
        }))
      })

  }

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
