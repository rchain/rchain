package coop.rchain.rholang.interpreter.normalizer

import coop.rchain.rholang.interpreter.normalizer.instances.ProcNormalizer

trait Normalizer[F[_], S, I, T, A] {
  def normalize(p: S, input: I)(implicit env: Map[String, A]): F[T]
}

object Normalizer extends ProcNormalizer {
  def apply[F[_], S, I, T, A](
      implicit instance: Normalizer[F, S, I, T, A]
  ): Normalizer[F, S, I, T, A] = instance
}
