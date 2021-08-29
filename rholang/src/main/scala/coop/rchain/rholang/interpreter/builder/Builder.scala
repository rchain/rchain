package coop.rchain.rholang.interpreter.builder

import cats.effect.Sync
import coop.rchain.rholang.interpreter.builder.instances.{ADTBuilderInstances, ASTBuilderInstances}

trait ASTBuilder[F[_], S, T] {
  def build(source: S): F[T]
}

object ASTBuilder extends ASTBuilderInstances {
  def apply[F[_], S, T](implicit instance: ASTBuilder[F, S, T]): ASTBuilder[F, S, T] =
    instance
}

trait ADTBuilder[F[_], S, T] {
  def buildWithEnv(source: S, normalizerEnv: Map[String, T]): F[T]
}

object ADTBuilder extends ADTBuilderInstances {
  def apply[F[_], S, T](implicit instance: ADTBuilder[F, S, T]): ADTBuilder[F, S, T] =
    instance
}
