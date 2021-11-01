package coop.rchain.rholang.interpreter

import cats._
import cats.arrow.FunctionK
import cats.data._
import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import cats.mtl._
import cats.mtl.implicits._
import coop.rchain.catscontrib.MonadError_
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.{InterpreterError, OutOfPhlogistonsError}

import scala.collection.immutable.Stream

package object matcher {

  type FreeMap = Map[Int, Par]

  type MatcherMonadT[F[_], A] = StateT[StreamT[F, ?], FreeMap, A]

  import coop.rchain.rholang.interpreter.matcher.StreamT

  implicit def matcherMonadCostLog[F[_]: Monad: Sync: _cost](): _cost[MatcherMonadT[F, ?]] =
    λ[F ~> MatcherMonadT[F, ?]](fa => StateT.liftF(StreamT.liftF(fa)))

  // The naming convention means: this is an effect-type alias.
  // Will be used similarly to capabilities, but for more generic and probably low-level/implementation stuff.
  // Adopted from: http://atnos-org.github.io/eff/org.atnos.site.Tutorial.html#write-an-interpreter-for-your-program
  type _freeMap[F[_]] = MonadState[F, FreeMap]
  type _short[F[_]]   = MonadError_[F, Unit] //arises from and corresponds to the OptionT/StreamT in the stack

  // Implicit summoner methods, just like `Monad.apply` on `Monad`'s companion object.
  def _freeMap[F[_]](implicit ev: _freeMap[F]): _freeMap[F] = ev
  def _short[F[_]](implicit ev: _short[F]): _short[F]       = ev

  private[matcher] def run[F[_]: Monad, A](
      f: MatcherMonadT[F, A]
  ): F[Stream[(FreeMap, A)]] =
    StreamT.run(f.run(emptyMap))

  private[rholang] def runFirst[F[_]: Monad, A](
      f: MatcherMonadT[F, A]
  ): F[Option[(FreeMap, A)]] =
    StreamT
      .run(StreamT.dropTail(f.run(emptyMap)))
      .map(_.headOption)

  private[matcher] def attemptOpt[F[_], A](
      f: F[A]
  )(implicit short: _short[F]): F[Option[A]] = {
    import short.instance
    f.attempt.map(_.fold(_ => None, Some(_)))
  }

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
