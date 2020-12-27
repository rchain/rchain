package coop.rchain.dag

import cats.syntax.all._
import cats.{Applicative, Monad}

import scala.collection.Seq

trait DAGReader[F[_], A] {
  def children(vertex: A): F[Option[Set[A]]]
  def parents(vertex: A): F[Option[Set[A]]]
}

trait DAGUpdater[F[_], A] {
  def addEdge(child: A, parent: A): F[InMemDAG[F, A]]
  def remove(vertex: A): F[Unit]
}

object DAGReader {

  /**
    * branch = tip block + all descendants through which tip is connected to base block.
    * @return sequence of blocks inside branch
    */
  def computeBranch[F[_]: Monad, A](tip: A, base: A, dag: DAGReader[F, A]): F[(A, Seq[A])] =
    for {
      // tip + all ancestors down to LFB block height
      // traverse from tip to LFB through parents, save blocks visited
      ancestorsSet <- (Seq(tip), Seq(tip)).tailRecM[F, Seq[A]] {
                       case (acc, blocksToProceed) =>
                         blocksToProceed.toList match {
                           case Nil =>
                             acc.asRight[(Seq[A], Seq[A])].pure[F]
                           case blocks =>
                             for {
                               nextBlocks <- blocks
                                              .traverse(
                                                b =>
                                                  dag
                                                    .parents(b)
                                                    .map(_.getOrElse(Set.empty))
                                              )
                                              .map(_.flatten)
                               // stop when base block is met
                               next = if (nextBlocks.contains(base)) Seq.empty else nextBlocks
                             } yield (acc ++ next, next).asLeft[Seq[A]]
                         }
                     }
      // blocks from tipSet connected to base
      branchSet <- (Seq(base), Seq(base)).tailRecM[F, Seq[A]] {
                    case (acc, blocksToProceed) =>
                      blocksToProceed.toList match {
                        case Nil =>
                          acc.asRight[(Seq[A], Seq[A])].pure[F]
                        case blocks =>
                          for {
                            children <- blocks
                                         .traverse(
                                           b =>
                                             dag
                                               .children(b)
                                               .map(_.getOrElse(Set.empty))
                                         )
                                         .map(_.flatten)
                            childrenFromTipSet = children.filter(ancestorsSet.contains)
                          } yield (acc ++ childrenFromTipSet, childrenFromTipSet).asLeft[Seq[A]]
                      }
                  }
    } yield (tip, branchSet)
}

final case class InMemDAG[F[_]: Applicative, A](
    childrenMap: Map[A, Set[A]],
    parentsMap: Map[A, Set[A]]
) extends DAGReader[F, A]
    with DAGUpdater[F, A] {
  override def children(vertex: A): F[Option[Set[A]]] = childrenMap.get(vertex).pure[F]
  override def parents(vertex: A): F[Option[Set[A]]]  = parentsMap.get(vertex).pure[F]
  override def addEdge(child: A, parent: A): F[InMemDAG[F, A]] =
    this
      .copy(
        childrenMap =
          childrenMap.updated(parent, childrenMap.getOrElse(parent, Set.empty[A]) + child),
        parentsMap = childrenMap.updated(parent, parentsMap.getOrElse(child, Set.empty[A]) + parent)
      )
      .pure[F]

  override def remove(vertex: A): F[Unit] = ???
}
