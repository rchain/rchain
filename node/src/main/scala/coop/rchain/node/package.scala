package coop.rchain

import cats.data.EitherT
import cats.syntax.applicative._

import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.eitherT._
import coop.rchain.comm.CommError

import monix.eval.Task

package object node {

  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = Task[A]

  implicit class TaskEffectOps[A](t: Task[A]) {
    def toEffect: Effect[A] = t
  }
}
