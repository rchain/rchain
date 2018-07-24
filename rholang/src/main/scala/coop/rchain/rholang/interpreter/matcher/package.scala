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

final case class StreamT[F[_], A](value: F[Stream[A]]) {
  def ap[B](f: F[A => B])(implicit F: Monad[F]): StreamT[F, B] =
    StreamT(F.flatMap(f)(ff => F.map(value)((stream: Stream[A]) => stream.map(ff))))

  def map[B](f: A => B)(implicit F: Functor[F]): StreamT[F, B] =
    StreamT(F.map(value)(_.map(f)))

  def flatMap[B](f: A => StreamT[F, B])(implicit F: Monad[F]): StreamT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Stream[B]])(implicit F: Monad[F]): StreamT[F, B] =
    StreamT(Monad[F].flatMap(value)(astream => astream.flatTraverse(a => f(a))))
}

object StreamT extends StreamTInstances0 {
  def pure[F[_], A](single: A)(implicit F: Applicative[F]): StreamT[F, A] =
    StreamT(F.pure(Stream(single)))
}

trait StreamTInstances0 {
  implicit def streamTMonad[F[_]: Monad]: Monad[StreamT[F, ?]] = new Monad[StreamT[F, ?]] {
    override def pure[A](x: A): StreamT[F, A] = StreamT(Monad[F].pure(Stream(x)))

    override def flatMap[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]): StreamT[F, B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => StreamT[F, Either[A, B]]): StreamT[F, B] =
      f(a).flatMap {
        case Right(b) => StreamT.pure(b)
        case Left(e)  => tailRecM(e)(f)
      }
  }
}
