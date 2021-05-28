package coop.rchain.dag

import cats.Show
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.dag.DagOps.bfTraverseF
import coop.rchain.shared.syntax._
import fs2.Stream

trait DagReaderSyntax {
  implicit final def syntaxDagReader[F[_], A](
      dag: DagReader[F, A]
  ): DagReaderOps[F, A] = new DagReaderOps[F, A](dag)
}

final class DagReaderOps[F[_], A](
    private val dag: DagReader[F, A]
) extends AnyVal {

  def parentsUnsafe(item: A)(
      implicit sync: Sync[F],
      show: Show[A]
  ): F[Seq[A]] = {
    def errMsg = s"Parents lookup failed: DAG is missing ${item.show}"
    dag.parents(item) >>= (_.liftTo(DagReader.VertexDoesNotExist(errMsg)))
  }

  def contains(item: A)(implicit sync: Sync[F]): F[Boolean] =
    dag.parents(item).map(_.isDefined) ||^ dag.children(item).map(_.isDefined)

  def mainParent(item: A)(implicit sync: Sync[F]): F[Option[A]] =
    dag.parents(item).map(_.flatMap(_.headOption))

  def mainParentUnsafe(item: A)(
      implicit sync: Sync[F],
      show: Show[A]
  ): F[A] = {
    def errMsg = s"Unsafe call for parents for block with no parents: ${item.show}"
    mainParent(item) >>= (_.liftTo(DagReader.VertexDoesNotExist(errMsg)))
  }

  def isInMainChain(
      descendant: A,
      ancestor: A,
      height: A => F[Long]
  )(implicit sync: Sync[F], show: Show[A]): F[Boolean] =
    descendant.tailRecM { d =>
      if (d == ancestor) true.asRight[A].pure[F]
      else
        for {
          dHeight <- height(d)
          aHeight <- height(ancestor)
          r <- if (dHeight <= aHeight)
                false.asRight[A].pure[F]
              else
                mainParentUnsafe(d).map(_.asLeft[Boolean])
        } yield r
    }
}
