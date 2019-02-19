package coop.rchain.rholang.interpreter

import cats.{Functor, Monad, MonadError}
import cats.arrow.FunctionK
import cats.data.StateT
import cats.implicits._
import cats.mtl.implicits._
import cats.mtl.{FunctorRaise, MonadState}
import coop.rchain.catscontrib.MonadError_
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.{InterpreterError, OutOfPhlogistonsError}

import scala.collection.immutable.Stream

package object matcher {

  type FreeMap = Map[Int, Par]

  //FreeMap => Cost => Either[InterpreterError, (Cost, Stream[(FreeMap, A)])]
  type NonDetFreeMapWithCost[A] = StateT[StreamWithCost, FreeMap, A]
  type StreamWithCost[A]        = StreamT[ErroredOrCost, A]
  type ErroredOrCost[A]         = StateT[Err, Cost, A]
  type Err[A]                   = Either[InterpreterError, A]

  // MatcherMonadT[Err, A] is equivalent to NonDetFreeMapWithCost[A]. Scalac is too dumb to notice though.
  type MatcherMonadT[F[_], A]   = StateT[StreamWithCostT[F, ?], FreeMap, A]
  type StreamWithCostT[F[_], A] = StreamT[StateT[F, Cost, ?], A]

  // The naming convention means: this is an effect-type alias.
  // Will be used similarly to capabilities, but for more generic and probably low-level/implementation stuff.
  // Adopted from: http://atnos-org.github.io/eff/org.atnos.site.Tutorial.html#write-an-interpreter-for-your-program
  type _freeMap[F[_]] = MonadState[F, FreeMap]
  type _short[F[_]]   = MonadError_[F, Unit] //arises from and corresponds to the OptionT/StreamT in the stack

  // Implicit summoner methods, just like `Monad.apply` on `Monad`'s companion object.
  def _freeMap[F[_]](implicit ev: _freeMap[F]): _freeMap[F] = ev
  def _short[F[_]](implicit ev: _short[F]): _short[F]       = ev

  private[matcher] def runFirstWithCost[F[_]: Monad, A](
      f: MatcherMonadT[F, A],
      initCost: Cost
  ): F[(Cost, Option[(FreeMap, A)])] =
    StreamT.run(StreamT.dropTail(f.run(emptyMap))).map(_.headOption).run(initCost)

  private[matcher] def attemptOpt[F[_], A](
      f: F[A]
  )(implicit short: _short[F]): F[Option[A]] = {
    import short.instance
    f.attempt.map(_.fold(_ => None, Some(_)))
  }

  object NonDetFreeMapWithCost {

    class NonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) {

      def runWithCost(
          initCost: Cost
      ): Either[InterpreterError, (Cost, Stream[(FreeMap, A)])] =
        runFreeMapAndStream.run(initCost)

      def runFirstWithCost(
          initCost: Cost
      ): Either[InterpreterError, (Cost, Option[(FreeMap, A)])] =
        runFreeMapAndStream.map(_.headOption).run(initCost)

      private def runFreeMapAndStream: ErroredOrCost[Stream[(FreeMap, A)]] =
        StreamT.run(s.run(Map.empty))

    }

    implicit def toNonDetFreeMapWithCostOps[A](s: NonDetFreeMapWithCost[A]) =
      new NonDetFreeMapWithCostOps[A](s)

  }

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
