package coop.rchain.shared

import cats._
import cats.syntax.all._

import scala.collection.immutable.{List, Set}

sealed abstract class StreamT[F[_], +A] { self =>

  def ++[AA >: A](other: StreamT[F, AA])(implicit functor: Functor[F]): StreamT[F, AA] =
    self match {
      case SCons(curr, lazyTail) => StreamT.cons(curr, lazyTail.map(_.map(_ ++ other)))
      case SLazy(lazyTail)       => StreamT.delay(lazyTail.map(_.map(_ ++ other)))
      case _: SNil[F]            => other
    }

  def drop(n: Int)(implicit functor: Functor[F]): StreamT[F, A] =
    if (n > 0) self match {
      case SCons(_, lazyTail) => StreamT.delay(lazyTail.map(_.map(_.drop(n - 1))))
      case SLazy(lazyTail)    => StreamT.delay(lazyTail.map(_.map(_.drop(n))))
      case _: SNil[F]         => StreamT.empty[F, A]
    } else self

  def filter(p: A => Boolean)(implicit functor: Functor[F]): StreamT[F, A] = self match {
    case SCons(curr, lazyTail) =>
      if (p(curr)) StreamT.cons(curr, lazyTail.map(_.map(_.filter(p))))
      else StreamT.delay(lazyTail.map(_.map(_.filter(p))))
    case SLazy(lazyTail) => StreamT.delay(lazyTail.map(_.map(_.filter(p))))
    case _: SNil[F]      => StreamT.empty[F, A]
  }

  def filterF[AA >: A](p: A => F[Boolean])(implicit monad: Monad[F]): F[StreamT[F, AA]] =
    self match {
      case SCons(curr, lazyTail) =>
        p(curr).map { cond =>
          if (cond)
            StreamT.cons(curr, lazyTail.map(_.flatMap(_.filterF(p))))
          else
            SLazy(lazyTail.map(_.flatMap(_.filterF(p))))
        }
      case SLazy(lazyTail) =>
        monad.pure[StreamT[F, AA]](SLazy(lazyTail.map(_.flatMap(_.filterF(p)))))
      case _: SNil[F] => StreamT.empty[F, AA].pure[F]
    }

  def find[AA >: A](p: A => Boolean)(implicit monad: Monad[F]): F[Option[AA]] = self match {
    case SCons(curr, lazyTail) =>
      if (p(curr)) monad.pure[Option[AA]](curr.some)
      else lazyTail.value.flatMap(_.find(p))

    case SLazy(lazyTail) => lazyTail.value.flatMap(_.find(p))

    case _: SNil[F] => none[AA].pure[F]
  }

  def findF[AA >: A](p: A => F[Boolean])(implicit monad: Monad[F]): F[Option[AA]] = self match {
    case SCons(curr, lazyTail) =>
      p(curr).flatMap { found =>
        if (found) monad.pure[Option[AA]](curr.some)
        else lazyTail.value.flatMap(_.findF(p))
      }

    case SLazy(lazyTail) => lazyTail.value.flatMap(_.findF(p))

    case _: SNil[F] => none[AA].pure[F]
  }

  def contains[AA >: A](x: AA)(implicit monad: Monad[F]): F[Boolean] =
    find(_ == x).map(_.isDefined)

  def flatMap[B](f: A => StreamT[F, B])(implicit monad: Monad[F]): StreamT[F, B] = self match {
    case SCons(curr, lazyTail) =>
      f(curr) match {
        case SCons(newHead, mappedLazyTail) =>
          val newLazyTail = StreamT.flatMapHelper(f, lazyTail, mappedLazyTail)
          StreamT.cons(newHead, newLazyTail)

        case SLazy(mappedLazyTail) =>
          StreamT.delay(StreamT.flatMapHelper(f, lazyTail, mappedLazyTail))

        case _: SNil[F] => StreamT.delay(lazyTail.map(_.map(_.flatMap(f))))
      }
    case SLazy(lazyTail) => StreamT.delay(lazyTail.map(_.map(_.flatMap(f))))

    case _: SNil[F] => StreamT.empty[F, B]
  }

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit monad: Monad[F]): F[B] = self match {
    case SCons(curr, lazyTail) =>
      val newB = f(b, curr)
      lazyTail.value.flatMap(_.foldLeft(newB)(f))

    case SLazy(lazyTail) => lazyTail.value.flatMap(_.foldLeft(b)(f))

    case _: SNil[F] => b.pure[F]
  }

  def foldLeftF[B](b: B)(f: (B, A) => F[B])(implicit monad: Monad[F]): F[B] = self match {
    case SCons(curr, lazyTail) =>
      for {
        newB   <- f(b, curr)
        result <- lazyTail.value.flatMap(_.foldLeftF(newB)(f))
      } yield result

    case SLazy(lazyTail) => lazyTail.value.flatMap(_.foldLeftF(b)(f))

    case _: SNil[F] => b.pure[F]
  }

  def foldRight[B](lb: Eval[B])(
      f: (A, Eval[F[B]]) => Eval[F[B]]
  )(implicit monad: Monad[F], traverse: Traverse[F]): Eval[F[B]] = self match {
    case SCons(curr, lazyTail) =>
      val mappedLazyTail = lazyTail.flatMap { tailF =>
        monad.map(tailF)(_.foldRight(lb)(f)).flatSequence
      }
      f(curr, mappedLazyTail)

    case SLazy(lazyTail) =>
      lazyTail.flatMap { tailF =>
        monad.map(tailF)(_.foldRight(lb)(f)).flatSequence
      }

    case _: SNil[F] => lb.map(_.pure[F])
  }

  def foreach(f: A => F[Unit])(implicit monad: Monad[F]): F[Unit] = self match {
    case SCons(curr, tailF) =>
      for {
        _        <- f(curr)
        lazyTail <- tailF.value
        _        <- lazyTail.foreach(f)
      } yield ()

    case SLazy(lazyTail) => lazyTail.value.flatMap(_.foreach(f))

    case _: SNil[F] => ().pure[F]
  }

  def head[AA >: A](implicit monadError: MonadError[F, Throwable]): F[AA] = self match {
    case SCons(head, _)  => monadError.pure[AA](head)
    case SLazy(lazyTail) => lazyTail.value.flatMap(_.head)
    case _: SNil[F] =>
      monadError.raiseError[AA](new Exception("Head on empty StreamT!"))
  }

  def map[B](f: A => B)(implicit functor: Functor[F]): StreamT[F, B] = self match {
    case SCons(curr, lazyTail) => StreamT.cons(f(curr), lazyTail.map(_.map(_.map(f))))
    case SLazy(lazyTail)       => StreamT.delay(lazyTail.map(_.map(_.map(f))))
    case _: SNil[F]            => StreamT.empty[F, B]
  }

  def mapF[B](f: A => F[B])(implicit functor: Functor[F]): StreamT[F, B] = self match {
    case SCons(curr, lazyTail) =>
      StreamT.delay(
        Eval.now(
          for {
            newB <- f(curr)
          } yield StreamT.cons(newB, lazyTail.map(_.map(_.mapF(f))))
        )
      )
    case SLazy(lazyTail) => StreamT.delay(lazyTail.map(_.map(_.mapF(f))))
    case _: SNil[F]      => StreamT.empty[F, B]
  }

  def tail(implicit applicativeError: ApplicativeError[F, Throwable]): StreamT[F, A] = self match {
    case SCons(_, lazyTail) => StreamT.delay(lazyTail)
    case SLazy(lazyTail)    => StreamT.delay(lazyTail.map(_.map(_.tail)))
    case _: SNil[F] =>
      StreamT.delay(
        Eval
          .now(applicativeError.raiseError[StreamT[F, A]](new Exception("Tail on empty StreamT!")))
      )
  }

  def take(n: Int)(implicit functor: Functor[F]): StreamT[F, A] =
    if (n > 0) self match {
      case SCons(curr, lazyTail) => StreamT.cons(curr, lazyTail.map(_.map(_.take(n - 1))))
      case SLazy(lazyTail)       => StreamT.delay(lazyTail.map(_.map(_.take(n))))
      case _: SNil[F]            => StreamT.empty[F, A]
    } else StreamT.empty[F, A]

  def takeWhile(p: A => Boolean)(implicit functor: Functor[F]): StreamT[F, A] = self match {
    case SCons(curr, lazyTail) if p(curr) => StreamT.cons(curr, lazyTail.map(_.map(_.takeWhile(p))))
    case SLazy(lazyTail)                  => StreamT.delay(lazyTail.map(_.map(_.takeWhile(p))))
    case _                                => StreamT.empty[F, A]
  }

  def dropWhile(p: A => Boolean)(implicit functor: Functor[F]): StreamT[F, A] = self match {
    case SCons(curr, lazyTail) =>
      if (p(curr)) StreamT.delay(lazyTail.map(_.map(_.dropWhile(p)))) else self
    case SLazy(lazyTail) => StreamT.delay(lazyTail.map(_.map(_.dropWhile(p))))
    case _: SNil[F]      => StreamT.empty[F, A]
  }

  def toList[AA >: A](implicit monad: Monad[F]): F[List[AA]] = self match {
    case SCons(curr, lazyTail) =>
      for {
        stail <- lazyTail.value
        ltail <- stail.toList[AA]
      } yield curr :: ltail

    case SLazy(lazyTail) => lazyTail.value.flatMap(_.toList)

    case _: SNil[F] => List.empty[AA].pure[F]
  }

  def toSet[AA >: A](implicit monad: Monad[F]): F[Set[AA]] = self match {
    case SCons(curr, lazyTail) =>
      for {
        stail <- lazyTail.value
        ltail <- stail.toSet[AA]
      } yield ltail + curr

    case SLazy(lazyTail) => lazyTail.value.flatMap(_.toSet)

    case _: SNil[F] => Set.empty[AA].pure[F]
  }

  def zip[B](other: StreamT[F, B])(implicit monad: Monad[F]): StreamT[F, (A, B)] =
    (self, other) match {
      case (SCons(a, lazyTailA), SCons(b, lazyTailB)) =>
        val tailB = StreamT.delay(lazyTailB)
        StreamT.cons((a, b), lazyTailA.map(_.map(_.zip[B](tailB))))

      case (streamA: SCons[F, A], SLazy(lazyTailB)) =>
        StreamT.delay(lazyTailB.map(_.map(tailB => streamA.zip(tailB))))

      case (SLazy(lazyTailA), streamB: SCons[F, B]) =>
        StreamT.delay(lazyTailA.map(_.map(tailA => tailA.zip(streamB))))

      case (SLazy(lazyTailA), SLazy(lazyTailB)) =>
        StreamT.delay(
          for {
            tailAF <- lazyTailA
            tailBF <- lazyTailB
          } yield for {
            tailA <- tailAF
            tailB <- tailBF
          } yield tailA.zip(tailB)
        )

      case (_: SNil[F], _) => StreamT.empty[F, (A, B)]

      case (_, _: SNil[F]) => StreamT.empty[F, (A, B)]
    }
}
final case class SCons[F[_], A](curr: A, lazyTail: Eval[F[StreamT[F, A]]]) extends StreamT[F, A]
final case class SLazy[F[_], A](lazyTail: Eval[F[StreamT[F, A]]])          extends StreamT[F, A]
final class SNil[F[_]]                                                     extends StreamT[F, Nothing]

object StreamT {
  type STail[F[_], A] = Eval[F[StreamT[F, A]]]

  def empty[F[_], A]: StreamT[F, A]                                = new SNil[F]
  def cons[F[_], A](curr: A, lazyTail: STail[F, A]): StreamT[F, A] = SCons(curr, lazyTail)
  def delay[F[_], A](lazyTail: STail[F, A]): StreamT[F, A]         = SLazy(lazyTail)

  def fromList[F[_], A](listF: F[List[A]])(implicit applicative: Applicative[F]): StreamT[F, A] = {
    def build(list: List[A]): StreamT[F, A] = list match {
      case head :: tail => StreamT.cons(head, Eval.later(build(tail).pure[F]))
      case Nil          => StreamT.empty[F, A]
    }

    StreamT.delay(Eval.now(listF.map(build(_))))
  }

  def continually[F[_]: Applicative, A](f: => F[A]): StreamT[F, A] = {
    def build: F[StreamT[F, A]] =
      for {
        value <- f
      } yield SCons(value, Eval.always(build))
    StreamT.delay(Eval.now(build))
  }

  private def flatMapHelper[F[_], A, B, BB <: B](
      f: A => StreamT[F, B],
      lazyTail: STail[F, A],
      mappedLazyTail: STail[F, BB]
  )(implicit monad: Monad[F]): STail[F, B] =
    for {
      tailF       <- lazyTail
      mappedTailF <- mappedLazyTail
    } yield for {
      mappedTail <- mappedTailF
      lazyTail   <- tailF
    } yield mappedTail ++ lazyTail.flatMap[B](f)
}
