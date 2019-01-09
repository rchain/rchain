package coop.rchain.rholang.interpreter

import cats.{Alternative, MonadError, Monoid, MonoidK}
import cats.arrow.FunctionK
import cats.data.{OptionT, StateT}
import cats.implicits._
import cats.mtl.implicits._
import cats.mtl.MonadState
import coop.rchain.catscontrib.MonadTrans
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

  // The naming convention means: this is an effect-type alias.
  // Will be used similarly to capabilities, but for more generic and probably low-level/implementation stuff.
  // Adopted from: http://atnos-org.github.io/eff/org.atnos.site.Tutorial.html#write-an-interpreter-for-your-program
  type _freeMap[F[_]] = MonadState[F, FreeMap]
  type _cost[F[_]]    = MonadState[F, Cost]
  type _error[F[_]]   = MonadError[F, OutOfPhlogistonsError.type]
  type _short[F[_]]   = MonadError[F, Unit] //arises from and corresponds to the OptionT/StreamT in the stack

  // Implicit summoner methods, just like `Monad.apply` on `Monad`'s companion object.
  def _freeMap[F[_]](implicit ev: _freeMap[F]): _freeMap[F] = ev
  def _cost[F[_]](implicit ev: _cost[F]): _cost[F]          = ev
  def _error[F[_]](implicit ev: _error[F]): _error[F]       = ev
  def _short[F[_]](implicit ev: _short[F]): _short[F]       = ev

  private[matcher] def charge[F[_]](
      amount: Cost
  )(implicit cost: _cost[F], error: _error[F]): F[Unit] =
    for {
      currentCost <- cost.get
      newCost     = currentCost - amount
      _           <- cost.set(newCost)
      _           <- error.ensure(cost.get)(OutOfPhlogistonsError)(_.value >= 0)
    } yield ()

  private[matcher] def attemptOpt[F[_], A](f: F[A])(implicit short: _short[F]): F[Option[A]] =
    short.attempt(f).map(_.fold(_ => None, Some(_)))

  object OptionalFreeMapWithCost {

    class OptionalFreeMapWithCostOps[A](s: OptionalFreeMapWithCost[A]) {

      def charge(amount: Cost): OptionalFreeMapWithCost[A] =
        s.flatMap(matcher.charge[OptionalFreeMapWithCost](amount).as)

      def attemptOpt: OptionalFreeMapWithCost[Option[A]] =
        matcher.attemptOpt(s)

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

    def empty[A]: OptionalFreeMapWithCost[A] =
      MonadTrans[StateT[?[_], FreeMap, ?]].liftM(MonoidK[OptionWithCost].empty)

    def fromOption[A](option: Option[A]): OptionalFreeMapWithCost[A] =
      StateT.liftF(OptionT.fromOption(option))
  }

  object NonDetFreeMapWithCost {

    class NonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) {

      def charge(amount: Cost): NonDetFreeMapWithCost[A] =
        s.flatMap(matcher.charge[NonDetFreeMapWithCost](amount).as)

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
      MonadTrans[StateT[?[_], FreeMap, ?]].liftM(MonoidK[StreamWithCost].empty)

    def fromStream[A](stream: Stream[A]): NonDetFreeMapWithCost[A] =
      StateT.liftF(StreamT.fromStream(StateT.pure(stream)))
  }

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
