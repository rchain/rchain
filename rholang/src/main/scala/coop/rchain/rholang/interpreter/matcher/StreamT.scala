package coop.rchain.rholang.interpreter.matcher
import cats.mtl.lifting.MonadLayerControl
import cats.{
  ~>,
  Alternative,
  Applicative,
  FlatMap,
  Functor,
  FunctorFilter,
  Monad,
  MonadError,
  MonoidK
}
import cats.data.OptionT
import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.rholang.interpreter.matcher.StreamT.{SCons, SNil, Step}

import scala.collection.immutable.LazyList
import scala.util.{Left, Right}
import cats.effect.Ref
import cats.effect.kernel.{CancelScope, MonadCancel, Poll}

import scala.concurrent.duration.FiniteDuration

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

  def fromStream[F[_]: Applicative, A](fs: F[LazyList[A]]): StreamT[F, A] = {

    def next(curr: LazyList[A]): Step[F, A] =
      curr match {
        case LazyList() => SNil()
        case cons       => SCons(cons.head, StreamT(delay[F, Step[F, A]](next(cons.tail))))
      }

    StreamT[F, A](Functor[F].map(fs)(next))
  }

  //This should delay the computation for most stacksafe monads
  //TODO consider doing the delay only for `F: Defer` to avoid the useless map
  private def delay[F[_]: Applicative, A](a: => A): F[A] =
    Functor[F].map(Applicative[F].unit)(_ => a)

  def run[F[_]: Monad, A](s: StreamT[F, A]): F[LazyList[A]] = {
    val F = Monad[F]
    F.flatMap(s.next) {
      case SNil()            => F.pure(LazyList())
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

    implicit override def apply[G[_]: Monad]: Monad[StreamT[G, *]] =
      streamTMonad[G]
  }

  implicit def streamTAlternative[F[_]: Monad]: Alternative[StreamT[F, *]] =
    new Alternative[StreamT[F, *]] with StreamTMonad[F] {

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

  private[matcher] def streamTMonad[F[_]](implicit F0: Monad[F]): Monad[StreamT[F, *]] =
    new StreamTMonad[F]() {
      implicit val F = F0
    }

}

private trait StreamTMonad[F[_]] extends Monad[StreamT[F, *]] {

  implicit def F: Monad[F]

  private lazy val effectMonoid = MonoidK[StreamT[F, *]]

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
  ): MonadError[StreamT[F, *], E] =
    new StreamTMonadError[F, E] { implicit val F = F0 }

  implicit def catsDataMonadErrorMonadForStreamT[F[_]](
      implicit F0: Monad[F]
  ): MonadError[StreamT[F, *], Unit] =
    new StreamTMonadErrorMonad[F] {
      implicit val F = F0
    }
}

private trait StreamTMonadError[F[_], E] extends MonadError[StreamT[F, *], E] with StreamTMonad[F] {

  override def F: MonadError[F, E]

  override def raiseError[A](e: E): StreamT[F, A] =
    StreamT.liftF(F.raiseError[A](e))(F)

  override def handleErrorWith[A](fa: StreamT[F, A])(f: E => StreamT[F, A]): StreamT[F, A] =
    StreamT(F.handleErrorWith(fa.next)(f(_).next))

}

trait StreamTInstances2 {
  private[matcher] type of[F[_], G[_]] = { type l[A] = F[G[A]] }
  private[matcher] type StreamTC[M[_]] = { type l[A] = StreamT[M, A] }

  implicit final def streamMonadLayerControl[M[_]](
      implicit M: Monad[M]
  ): MonadLayerControl.Aux[StreamTC[M]#l, M, LazyList] =
    new MonadLayerControl[StreamTC[M]#l, M] {
      type State[A] = LazyList[A]

      val outerInstance: Monad[StreamTC[M]#l] =
        StreamT.streamTMonad

      val innerInstance: Monad[M] = M

      def layerMapK[A](ma: StreamT[M, A])(trans: M ~> M): StreamT[M, A] = StreamT(trans(ma.next))

      def layer[A](inner: M[A]): StreamT[M, A] = StreamT.liftF(inner)

      def restore[A](state: LazyList[A]): StreamT[M, A] =
        StreamT.fromStream[M, A](innerInstance.pure(state))

      def layerControl[A](cps: (StreamTC[M]#l ~> (M of LazyList)#l) => M[A]): StreamT[M, A] =
        StreamT.liftF(cps(new (StreamTC[M]#l ~> (M of LazyList)#l) {
          def apply[X](fa: StreamT[M, X]): M[LazyList[X]] = StreamT.run(fa)
        }))

      def zero[A](state: LazyList[A]): Boolean = state.isEmpty
    }

  implicit def streamTSync[F[_]](
      implicit F0: Sync[F],
      M0: FlatMap[StreamT[F, *]]
  ): Sync[StreamT[F, *]] =
    new StreamTSync[F]() {

      override def suspend[A](hint: Sync.Type)(thunk: => A): StreamT[F, A] =
        StreamT.liftF(F0.delay(thunk))

      override def monotonic: StreamT[F, FiniteDuration] = StreamT.liftF(F0.monotonic)

      override def realTime: StreamT[F, FiniteDuration] = StreamT.liftF(F0.realTime)

      override def rootCancelScope: CancelScope = F0.rootCancelScope

      override def forceR[A, B](fa: StreamT[F, A])(fb: StreamT[F, B]): StreamT[F, B] =
        M0.flatMap(fa)(_ => fb)

      override def uncancelable[A](body: Poll[StreamT[F, *]] => StreamT[F, A]): StreamT[F, A] = {
        val natT: Poll[StreamT[F, *]] = new Poll[StreamT[F, *]] {
          override def apply[B](fa: StreamT[F, B]): StreamT[F, B] = {
            val step: F[Step[F, B]] = fa.next.flatMap {
              case SCons(h, t) =>
                F0.uncancelable { nat: Poll[F] =>
                  nat(h.pure[F]).map(SCons[F, B](_, t))
                }
              case x @ SNil() => x.asInstanceOf[Step[F, B]].pure[F]
            }
            StreamT[F, B](step)
          }
        }
        body(natT)
      }

      override def canceled: StreamT[F, Unit] = StreamT.liftF(F0.canceled)

      override def onCancel[A](fa: StreamT[F, A], fin: StreamT[F, Unit]): StreamT[F, A] =
        StreamT[F, A](F0.onCancel(fa.next, fin.next.void))

      override def F: MonadError[F, Throwable] = F0
    }
}

private trait StreamTSync[F[_]] extends Sync[StreamT[F, *]] with StreamTMonadError[F, Throwable]

private trait StreamTMonadErrorMonad[F[_]]
    extends MonadError[StreamT[F, *], Unit]
    with StreamTMonad[F] {
  implicit def F: Monad[F]

  override def raiseError[A](e: Unit): StreamT[F, A] = StreamT.empty

  override def handleErrorWith[A](fa: StreamT[F, A])(f: Unit => StreamT[F, A]): StreamT[F, A] =
    StreamT(F.flatMap(fa.next) {
      case s @ SCons(_, _) => F.pure(s)
      case SNil()          => f(()).next
    })
}
