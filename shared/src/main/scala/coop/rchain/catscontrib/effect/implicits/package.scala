package coop.rchain.catscontrib.effect

import cats._
import cats.arrow.FunctionK
import cats.effect._
import cats.effect.kernel.CancelScope
import cats.syntax.all._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.util.control.NonFatal

package object implicits {
  // Sync typeclass implementation for cats.Eval datatype is required to use cats Eval for stack safe serialization of
  // Rholang types. This replaces (as part of attempt to abstract from concrete effect type)
  // monix.Сoeval that was used for this purpose before.
  implicit val sEval = new Sync[Eval] {
    override def suspend[A](hint: Sync.Type)(thunk: => A): Eval[A] = Eval.always(thunk)

    override def rootCancelScope: CancelScope = CancelScope.Cancelable

    override def forceR[A, B](fa: Eval[A])(fb: Eval[B]): Eval[B] = fa.flatMap(_ => fb)

    override def uncancelable[A](body: Poll[Eval] => Eval[A]): Eval[A] = {
      val poll: Poll[Eval] = FunctionK.id[Eval].asInstanceOf[Poll[Eval]]
      body(poll)
    }

    override def canceled: Eval[Unit] = Eval.Unit

    override def onCancel[A](fa: Eval[A], fin: Eval[Unit]): Eval[A] = fin.map(_.asInstanceOf[A])

    override def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Eval[Either[A, B]]): Eval[B] = a.tailRecM(f)

    override def pure[A](x: A): Eval[A] = Eval.now(x)

    override def monotonic: Eval[FiniteDuration] =
      Eval.always(FiniteDuration(java.lang.System.nanoTime, MILLISECONDS))

    override def realTime: Eval[FiniteDuration] =
      Eval.always(FiniteDuration(java.lang.System.currentTimeMillis, MILLISECONDS))

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    override def raiseError[A](e: Throwable): Eval[A] = Eval.later(throw e)

    override def handleErrorWith[A](fa: Eval[A])(f: Throwable => Eval[A]): Eval[A] =
      try {
        Eval.always(fa.value)
      } catch {
        case NonFatal(e) => f(e)
      }
  }

  // Fo use only in tests
  implicit val sId = new Async[Id] {
    override def suspend[A](hint: Sync.Type)(thunk: => A): Id[A] = thunk

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = a.tailRecM(f)

    override def monotonic: Id[FiniteDuration] =
      FiniteDuration(java.lang.System.nanoTime, MILLISECONDS)

    override def realTime: Id[FiniteDuration] =
      FiniteDuration(java.lang.System.currentTimeMillis, MILLISECONDS)

    override def forceR[A, B](fa: Id[A])(fb: Id[B]): Id[B] = fb

    override def uncancelable[A](body: Poll[Id] => Id[A]): Id[A] = {
      val poll: Poll[Id] = FunctionK.id[Eval].asInstanceOf[Poll[Id]]
      body(poll)
    }

    override def canceled: Id[Unit] = ()

    override def onCancel[A](fa: Id[A], fin: Id[Unit]): Id[A] = fin.asInstanceOf[A]

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    override def raiseError[A](e: Throwable): Id[A] = throw e

    override def handleErrorWith[A](fa: Id[A])(f: Throwable => Id[A]): Id[A] =
      try (fa)
      catch { case NonFatal(e) => f(e) }

    override def pure[A](x: A): Id[A] = x

    override def evalOn[A](fa: Id[A], ec: ExecutionContext): Id[A] = fa

    override def executionContext: Id[ExecutionContext] = scala.concurrent.ExecutionContext.global

    override def cont[K, R](body: Cont[Id, K, R]): Id[R] = ???

    protected override def sleep(time: FiniteDuration): Id[Unit] = Thread.sleep(time.toMillis)

    override def ref[A](a: A): Id[Ref[Id, A]] = ???

    override def deferred[A]: Id[Deferred[Id, A]] = ???

    override def start[A](fa: Id[A]): Id[Fiber[Id, Throwable, A]] = new Fiber[Id, Throwable, A] {
      override def cancel: Id[Unit]                    = ()
      override def join: Id[Outcome[Id, Throwable, A]] = Outcome.succeeded(fa)
    }

    override def cede: Id[Unit] = ()
  }
}
