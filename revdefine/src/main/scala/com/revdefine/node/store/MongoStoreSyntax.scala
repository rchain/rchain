package com.revdefine.node.store

import cats.effect.{Async, ContextShift}
import org.mongodb.scala.{FindObservable, SingleObservable}

trait MongoObservableSyntax {
  implicit final def mongoFindObservableSyntax[A](
      o: FindObservable[A]
  ): MongoFindObservableSyntaxOps[A] = new MongoFindObservableSyntaxOps[A](o)

  implicit final def mongoSingleObservableSyntax[A](
      o: SingleObservable[A]
  ): MongoSingleObservableSyntaxOps[A] = new MongoSingleObservableSyntaxOps(o)
}

final class MongoFindObservableSyntaxOps[A](private val o: FindObservable[A]) extends AnyVal {
  def liftToF[F[_]: Async: ContextShift]: F[Seq[A]] = Async.fromFuture(Async[F].delay(o.toFuture()))
}

final class MongoSingleObservableSyntaxOps[A](private val o: SingleObservable[A]) extends AnyVal {
  def liftToF[F[_]: Async: ContextShift]: F[A] = Async.fromFuture(Async[F].delay(o.head()))
}
