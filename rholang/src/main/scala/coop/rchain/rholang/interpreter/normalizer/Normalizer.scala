package coop.rchain.rholang.interpreter.normalizer

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.rholang.ast.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.interpreter.compiler.{DeBruijnLevelMap, IndexMapChain}
import coop.rchain.rholang.interpreter.compiler.Visit.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.interpreter.normalizer.instances.{ProcNormalizer, ProcSourceNormalizer}

trait Normalizer[F[_], S, I[_], T[_], A] {
  // TODO remove env
  def normalize(p: S, input: I[A])(implicit env: Map[String, A]): F[T[A]]
}

object Normalizer extends ProcNormalizer with ProcSourceNormalizer {
  def apply[F[_], S, I[_], T[_], A](
      implicit instance: Normalizer[F, S, I, T, A]
  ): Normalizer[F, S, I, T, A] = instance
}
