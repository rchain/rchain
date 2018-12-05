package coop.rchain.rholang.interpreter.matcher
import cats.implicits._
import cats.mtl.lifting.MonadLayerControl
import cats.{~>, Monad, MonadError, MonoidK}
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.rholang.interpreter.matcher.StreamT.{SCons, SNil, Step}

import scala.collection.immutable.Stream
import scala.collection.immutable.Stream.Cons
import scala.util.{Left, Right}

/**
  * Shamelessly transcribed minimal version of Gabriel Gonzalez's beginner-friendly ListT
  * https://github.com/Gabriel439/Haskell-List-Transformer-Library/blob/e9a1d19/src/List/Transformer.hs
  *
  * See also: http://www.haskellforall.com/2016/07/list-transformer-beginner-friendly-listt.html
  *
  * The monads resulting from applying this transformer are as lazy and stacksafe as the underlying F monad you pass.
  * Don't pass a strict monad and expect stacksafety. This, somewhat unlawfully, includes the behavior of tailRecM (!).
  * All is good if F is stacksafe though.
  *
  * @param next effectful computation of the next Step in the represented stream
  * @tparam F
  * @tparam A
  */
final case class StreamT[F[_], A](next: F[Step[F, A]])

object StreamT extends StreamTInstances0 {

  sealed trait Step[F[_], A]
  case class SNil[F[_], A]()                              extends Step[F, A]
  case class SCons[F[_], A](head: A, tail: StreamT[F, A]) extends Step[F, A]

  def empty[F[_]: Monad, A]: StreamT[F, A] =
    StreamT[F, A](Monad[F].pure(SNil()))

  def pure[F[_]: Monad, A](a: A): StreamT[F, A] =
    StreamT(Monad[F].pure(SCons(a, empty)))

  def liftF[F[_]: Monad, A](fa: F[A]): StreamT[F, A] =
    StreamT(fa.map(value => SCons(value, empty)))

  def fromStream[F[_]: Monad, A](fs: F[Stream[A]]): StreamT[F, A] = {

    def next(curr: Stream[A]): Step[F, A] =
      curr match {
        case Stream.Empty  => SNil()
        case cons: Cons[A] => SCons(cons.head, StreamT(delay[F, Step[F, A]](next(cons.tail))))
      }

    StreamT[F, A](fs.map(next))
  }

  //This should delay the computation for most stacksafe monads
  //TODO consider doing the delay only for `F: Defer` to avoid the useless map
  private def delay[F[_]: Monad, A](a: => A): F[A] =
    Monad[F].unit.map(_ => a)

  def run[F[_]: Monad, A](s: StreamT[F, A]): F[Stream[A]] =
    s.next.flatMap {
      case SNil()            => Monad[F].pure(Stream.Empty)
      case SCons(head, tail) => run(tail).map(t => head +: t)
    }
}

trait StreamTInstances0 extends StreamTInstances1 {

  implicit val streamTMonadTrans = new MonadTrans[StreamT] {
    override def liftM[G[_]: Monad, A](a: G[A]): StreamT[G, A] =
      StreamT.liftF(a)

    override implicit def apply[G[_]: Monad]: Monad[StreamT[G, ?]] =
      streamTMonad[G]
  }

  implicit def streamTMonoidK[F[_]: Monad]: MonoidK[StreamT[F, ?]] = new MonoidK[StreamT[F, ?]] {

    override def empty[A]: StreamT[F, A] = StreamT.empty

    override def combineK[A](x: StreamT[F, A], y: StreamT[F, A]): StreamT[F, A] = {
      val next: F[Step[F, A]] = x.next.flatMap {
        case SNil()            => y.next
        case SCons(head, tail) => Monad[F].pure(SCons(head, combineK(tail, y)))
      }
      StreamT(next)
    }

  }

  implicit def streamTMonad[F[_]](implicit F0: Monad[F]): Monad[StreamT[F, ?]] =
    new StreamTMonad[F]() {
      implicit val F = F0
    }

}

private trait StreamTMonad[F[_]] extends Monad[StreamT[F, ?]] {

  implicit def F: Monad[F]

  override def pure[A](x: A): StreamT[F, A] =
    StreamT.pure[F, A](x)

  override def flatMap[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]): StreamT[F, B] = {
    val next: F[Step[F, B]] = fa.next.flatMap {
      case SNil() => Monad[F].pure[Step[F, B]](SNil())
      case SCons(head, tail) =>
        val effectMonoid          = MonoidK[StreamT[F, ?]]
        val result: StreamT[F, B] = effectMonoid.combineK(f(head), tail.flatMap(f))
        result.next
    }
    StreamT(next)
  }

  override def tailRecM[A, B](a: A)(f: A => StreamT[F, Either[A, B]]): StreamT[F, B] =
    flatMap(f(a)) {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => pure(b)
    }

}

trait StreamTInstances1 {

  implicit def catsDataMonadErrorForStreamT[F[_], E](
      implicit F0: MonadError[F, E]
  ): MonadError[StreamT[F, ?], E] =
    new StreamTMonadError[F, E] { implicit val F = F0 }

}

private trait StreamTMonadError[F[_], E] extends MonadError[StreamT[F, ?], E] with StreamTMonad[F] {

  override def F: MonadError[F, E]

  override def raiseError[A](e: E): StreamT[F, A] =
    StreamT.liftF(F.raiseError[A](e))(F)

  override def handleErrorWith[A](fa: StreamT[F, A])(f: E => StreamT[F, A]): StreamT[F, A] =
    StreamT(F.handleErrorWith(fa.next)(f(_).next))

}
