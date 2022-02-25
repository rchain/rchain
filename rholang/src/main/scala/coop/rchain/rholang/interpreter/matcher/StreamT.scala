package coop.rchain.rholang.interpreter.matcher
import cats.mtl.lifting.MonadLayerControl
import cats.{~>, Alternative, Applicative, Functor, FunctorFilter, Monad, MonadError, MonoidK}
import cats.data.OptionT
import cats.effect.Sync
import cats.effect.concurrent.Ref
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
  final case class SNil[F[_], A]()                              extends Step[F, A]
  final case class SCons[F[_], A](head: A, tail: StreamT[F, A]) extends Step[F, A]

  def empty[F[_]: Applicative, A]: StreamT[F, A] =
    StreamT[F, A](Applicative[F].pure(SNil()))

  def pure[F[_]: Applicative, A](a: A): StreamT[F, A] =
    StreamT(Applicative[F].pure(SCons(a, empty)))

  def liftF[F[_]: Applicative, A](fa: F[A]): StreamT[F, A] =
    StreamT(Functor[F].map(fa)(value => SCons(value, empty)))

  def fromStream[F[_]: Applicative, A](fs: F[Stream[A]]): StreamT[F, A] = {

    def next(curr: Stream[A]): Step[F, A] =
      curr match {
        case Stream.Empty  => SNil()
        case cons: Cons[A] => SCons(cons.head, StreamT(delay[F, Step[F, A]](next(cons.tail))))
      }

    StreamT[F, A](Functor[F].map(fs)(next))
  }

  //This should delay the computation for most stacksafe monads
  //TODO consider doing the delay only for `F: Defer` to avoid the useless map
  private def delay[F[_]: Applicative, A](a: => A): F[A] =
    Functor[F].map(Applicative[F].unit)(_ => a)

  def run[F[_]: Monad, A](s: StreamT[F, A]): F[Stream[A]] = {
    val F = Monad[F]
    F.flatMap(s.next) {
      case SNil()            => F.pure(Stream.Empty)
      case SCons(head, tail) => F.map(run(tail))(t => head +: t)
    }
  }

  def dropTail[F[_]: Monad, A](fa: StreamT[F, A]): StreamT[F, A] = {
    val F = Monad[F]
    val next: F[Step[F, A]] = F.map(fa.next) {
      case SCons(head, _) => SCons(head, empty)
      case nil @ SNil()   => nil
    }
    StreamT(next)
  }
}

trait StreamTInstances0 extends StreamTInstances1 {

  implicit val streamTMonadTrans = new MonadTrans[StreamT] {
    override def liftM[G[_]: Monad, A](a: G[A]): StreamT[G, A] =
      StreamT.liftF(a)

    implicit override def apply[G[_]: Monad]: Monad[StreamT[G, ?]] =
      streamTMonad[G]
  }

  implicit def streamTAlternative[F[_]: Monad]: Alternative[StreamT[F, ?]] =
    new Alternative[StreamT[F, ?]] with StreamTMonad[F] {

      override val F = Monad[F]

      override def empty[A]: StreamT[F, A] = StreamT.empty

      override def combineK[A](x: StreamT[F, A], y: StreamT[F, A]): StreamT[F, A] = {
        val next: F[Step[F, A]] = F.flatMap(x.next) {
          case SNil()            => y.next
          case SCons(head, tail) => F.pure(SCons(head, combineK(tail, y)))
        }
        StreamT(next)
      }

    }

  private[matcher] def streamTMonad[F[_]](implicit F0: Monad[F]): Monad[StreamT[F, ?]] =
    new StreamTMonad[F]() {
      implicit val F = F0
    }

}

private trait StreamTMonad[F[_]] extends Monad[StreamT[F, ?]] {

  implicit def F: Monad[F]

  private lazy val effectMonoid = MonoidK[StreamT[F, ?]]

  override def pure[A](x: A): StreamT[F, A] =
    StreamT.pure[F, A](x)

  override def flatMap[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]): StreamT[F, B] = {
    val next: F[Step[F, B]] = F.flatMap(fa.next) {
      case SNil() => F.pure[Step[F, B]](SNil())
      case SCons(head, tail) =>
        val result: StreamT[F, B] = effectMonoid.combineK(f(head), flatMap(tail)(f))
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

trait StreamTInstances1 extends StreamTInstances2 {

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

trait StreamTInstances2 {
  private[matcher] type of[F[_], G[_]] = { type l[A] = F[G[A]] }
  private[matcher] type StreamTC[M[_]] = { type l[A] = StreamT[M, A] }

  implicit def catsDataMonadErrorMonadForStreamT[F[_]](
      implicit F0: Monad[F]
  ): MonadError[StreamT[F, ?], Unit] =
    new StreamTMonadErrorMonad[F] { implicit val F = F0 }

  implicit final def streamMonadLayerControl[M[_]](
      implicit M: Monad[M]
  ): MonadLayerControl.Aux[StreamTC[M]#l, M, Stream] =
    new MonadLayerControl[StreamTC[M]#l, M] {
      type State[A] = Stream[A]

      val outerInstance: Monad[StreamTC[M]#l] =
        StreamT.streamTMonad

      val innerInstance: Monad[M] = M

      def layerMapK[A](ma: StreamT[M, A])(trans: M ~> M): StreamT[M, A] = StreamT(trans(ma.next))

      def layer[A](inner: M[A]): StreamT[M, A] = StreamT.liftF(inner)

      def restore[A](state: Stream[A]): StreamT[M, A] =
        StreamT.fromStream[M, A](innerInstance.pure(state))

      def layerControl[A](cps: (StreamTC[M]#l ~> (M of Stream)#l) => M[A]): StreamT[M, A] =
        StreamT.liftF(cps(new (StreamTC[M]#l ~> (M of Stream)#l) {
          def apply[X](fa: StreamT[M, X]): M[Stream[X]] = StreamT.run(fa)
        }))

      def zero[A](state: Stream[A]): Boolean = state.isEmpty
    }

  implicit def streamTSync[F[_]](
      implicit F0: Sync[F],
      M0: Monad[StreamT[F, ?]],
      AL0: Alternative[StreamT[F, ?]]
  ): Sync[StreamT[F, ?]] =
    new StreamTSync[F]() {
      implicit val F  = F0
      implicit val M  = M0
      implicit val AL = AL0
    }
}

private trait StreamTSync[F[_]] extends Sync[StreamT[F, ?]] with StreamTMonadError[F, Throwable] {
  implicit def F: Sync[F]
  implicit def M: Monad[StreamT[F, ?]]
  implicit def AL: Alternative[StreamT[F, ?]]

  import cats.effect.ExitCase

  def bracketCase[A, B](acquire: StreamT[F, A])(use: A => StreamT[F, B])(
      release: (A, ExitCase[Throwable]) => StreamT[F, Unit]
  ): StreamT[F, B] =
    flatMap(StreamT.liftF(Ref.of[F, Boolean](false))) { ref =>
      StreamT(F.flatMap(F.bracketCase[Step[F, A], Step[F, B]](acquire.next) {
        case SNil() => F.pure(SNil())
        case SCons(head, tail) => {
          AL.combineK(use(head), M.flatMap(tail)(use)).next
        }
      } {
        case (SNil(), _) => F.pure(())
        case (SCons(head, _), ExitCase.Completed) => {
          F.flatMap(release(head, ExitCase.Completed).next) {
            case SNil()      => ref.set(true)
            case SCons(_, _) => F.unit
          }
        }
        case (SCons(head, _), ec) => {
          F.map(release(head, ec).next)(_ => ())
        }
      }) {
        case s @ SCons(_, _) => F.map(ref.get)(b => if (b) SNil() else s)
        case SNil()          => F.pure(SNil())
      })
    }

  def suspend[A](thunk: => StreamT[F, A]): StreamT[F, A] =
    StreamT(F.defer(thunk.next))

}

private trait StreamTMonadErrorMonad[F[_]]
    extends MonadError[StreamT[F, ?], Unit]
    with StreamTMonad[F] {
  implicit def F: Monad[F]

  override def raiseError[A](e: Unit): StreamT[F, A] = StreamT.empty

  override def handleErrorWith[A](fa: StreamT[F, A])(f: Unit => StreamT[F, A]): StreamT[F, A] =
    StreamT(F.flatMap(fa.next) {
      case s @ SCons(_, _) => F.pure(s)
      case SNil()          => f(()).next
    })
}
