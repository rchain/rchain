package coop.rchain.rholang.interpreter.normalizer

import coop.rchain.rholang.interpreter.normalizer.instances.ProcNormalizer

trait Normalizer[F[_], S, I[_], T[_], A] {
  def normalize(p: S, input: I[A])(implicit env: Map[String, A]): F[T[A]]
}

object Normalizer extends ProcNormalizer {
  def apply[F[_], S, I[_], T[_], A](
      implicit instance: Normalizer[F, S, I, T, A]
  ): Normalizer[F, S, I, T, A] = instance
}
