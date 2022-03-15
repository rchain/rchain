package coop.rchain.rholang.interpreter
import cats.Monad
import cats.arrow.FunctionK
import cats.mtl.lifting.FunctorLayerFunctor
import coop.rchain.rholang.interpreter.matcher.StreamT

/**
  * This is a lawless typeclass to abstract over types providing a `takeFirst()` method, as defined in
  * NonDetFreeMapWithCost's ops.
  *
  * @tparam F
  */
trait Splittable[F[_]] {

  def takeFirst[A](fa: F[A]): F[A]

}

object Splittable extends SplittableInstancesLowPriority {

  def apply[F[_]](implicit ev: Splittable[F]) = ev

  implicit class SplittableOps[F[_]: Splittable, A](self: F[A]) {

    def takeFirst(): F[A] =
      Splittable[F].takeFirst[A](self)
  }

  // This induction rule derives Splittable[F] for any monad transformer stack F
  // as long as its top layers provide a FunctorLayerFunctor up to a layer L that
  // has a Splittable[L] by itself.
  // The base case of the induction is in the SplittableInstancesLowPriority trait.
  // Most monad transformers (all I've seen?) provide a much stronger MonadLayerControl BTW.
  implicit def splittableInd[F[_], G[_]](
      implicit lift: FunctorLayerFunctor[F, G],
      under: Splittable[G]
  ): Splittable[F] = new Splittable[F] {
    override def takeFirst[A](fa: F[A]): F[A] =
      lift.layerMapK(fa)(new FunctionK[G, G] {
        override def apply[B](gb: G[B]): G[B] = under.takeFirst(gb)
      })
  }
}

trait SplittableInstancesLowPriority {

  implicit def streamTSplittable[F[_]: Monad]: Splittable[StreamT[F, *]] =
    new Splittable[StreamT[F, *]] {
      override def takeFirst[A](fa: StreamT[F, A]): StreamT[F, A] = StreamT.dropTail(fa)
    }

}
