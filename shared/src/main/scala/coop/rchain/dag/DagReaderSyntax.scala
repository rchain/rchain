package coop.rchain.dag

import cats.effect.Concurrent
import coop.rchain.dag.DagOps.bfTraverseF
import coop.rchain.shared.syntax._
import cats.syntax.all._
import fs2.Stream

trait DagReaderSyntax {
  implicit final def syntaxDagReader[F[_], A](
      dag: DagReader[F, A]
  ): DagReaderOps[F, A] = new DagReaderOps[F, A](dag)
}

final class DagReaderOps[F[_], A](
    private val dag: DagReader[F, A]
) extends AnyVal {

  def gatherAncestors(start: A, stopCondition: A => F[Boolean])(
      implicit concurrent: Concurrent[F]
  ): F[Set[A]] =
    bfTraverseF[F, A](List(start))(
      hash =>
        Stream(hash)
          .evalMap(h => dag.parents(h).map(_.getOrElse(Set.empty[A])))
          .flatMap(xs => Stream.fromIterator(xs.toIterator))
          .evalFilterAsyncProcBounded(v => stopCondition(v).not)
          .compile
          .toList
    ).toSet
}
