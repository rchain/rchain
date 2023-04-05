package coop.rchain.rholang.interpreter.accounting

import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import cats.mtl._
import cats.mtl.implicits._
import cats.effect.Ref

package object utils {

  def costLog[M[_]: Sync](): M[FunctorListen[M, Chain[Cost]]] =
    for {
      ref <- Ref.of(Chain.empty[Cost])
    } yield (new DefaultFunctorListen[M, Chain[Cost]] {
      override val functor: Functor[M]  = implicitly[Functor[M]]
      def tell(l: Chain[Cost]): M[Unit] = ref.modify(c => (c.concat(l), ()))
      def listen[A](fa: M[A]): M[(A, Chain[Cost])] =
        for {
          a <- fa
          r <- ref.get
        } yield ((a, r))
    })
}
