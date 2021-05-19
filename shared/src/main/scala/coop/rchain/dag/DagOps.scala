package coop.rchain.dag

import cats.effect.Concurrent
import cats.implicits._
import cats.{Applicative, Eval, Monad}
import coop.rchain.shared.StreamT

import scala.collection.{LinearSeq, Seq}
import scala.collection.generic.Sorted
import scala.collection.immutable.{HashSet, Queue}

trait DagReader[F[_], A] {
  def children(vertex: A): F[Option[Set[A]]]
  // parents should be ordered as we need a notion if main parent
  def parents(vertex: A): F[Option[Seq[A]]]
}

object DagReader {
  final case class VertexDoesNotExist(str: String) extends Exception
}

trait DagUpdater[F[_], A] {
  def addEdge(child: A, parent: A): F[InMemDAG[F, A]]
  def remove(vertex: A): F[Unit]
}

final case class InMemDAG[F[_]: Applicative, A](
    childrenMap: Map[A, Set[A]],
    parentsMap: Map[A, Seq[A]]
) extends DagReader[F, A]
    with DagUpdater[F, A] {
  override def children(vertex: A): F[Option[Set[A]]] =
    childrenMap.get(vertex).pure[F]
  override def parents(vertex: A): F[Option[Seq[A]]] = parentsMap.get(vertex).pure[F]
  override def addEdge(child: A, parent: A): F[InMemDAG[F, A]] =
    new InMemDAG[F, A](
      this.childrenMap.updated(parent, childrenMap.getOrElse(parent, Set.empty[A]) + child),
      this.parentsMap.updated(child, parentsMap.getOrElse(child, Seq.empty[A]) :+ parent)
    ).pure[F]

  override def remove(vertex: A): F[Unit] = ???
}

object DagOps {
  def bfTraverseF[F[_]: Monad, A](start: List[A])(neighbours: A => F[List[A]]): StreamT[F, A] = {
    def build(q: Queue[A], prevVisited: HashSet[A]): F[StreamT[F, A]] =
      if (q.isEmpty) StreamT.empty[F, A].pure[F]
      else {
        val (curr, rest) = q.dequeue
        if (prevVisited(curr)) build(rest, prevVisited)
        else
          for {
            ns      <- neighbours(curr)
            visited = prevVisited + curr
            newQ    = rest.enqueue[A](ns.filterNot(visited))
          } yield StreamT.cons(curr, Eval.always(build(newQ, visited)))
      }

    StreamT.delay(Eval.now(build(Queue.empty[A].enqueue[A](start), HashSet.empty[A])))
  }

  def bfTraverse[A](start: List[A])(neighbours: A => List[A]): Stream[A] = {
    def build(q: Queue[A], prevVisited: HashSet[A]): Stream[A] =
      if (q.isEmpty) Stream.empty[A]
      else {
        val (curr, rest) = q.dequeue
        if (prevVisited(curr)) build(rest, prevVisited)
        else {
          val ns      = neighbours(curr)
          val visited = prevVisited + curr
          val newQ    = rest.enqueue[A](ns.filterNot(visited))
          Stream.cons(curr, build(newQ, visited))
        }
      }

    build(Queue.empty[A].enqueue[A](start), HashSet.empty[A])
  }

  /**
    * Tip block + all descendants through which tip is connected to base block.
    * @param tip -  the very top vertex
    * @param base - the very bottom vertex
    * @param dag - dependency graph
    * @tparam F
    * @tparam A - type of vertex
    * @return Sequence of blocks inside branch, sorted from tip to base.
    *         Sorting of vertices at same level is not defined.
    */
  def computeTipToBaseBranch[F[_]: Monad, A](
      tip: A,
      base: A,
      dag: DagReader[F, A]
  ): F[LinearSeq[A]] =
    for {
      // tip + all ancestors down to LFB block height
      // traverse from tip to LFB through parents, save blocks visited
      tipBranch <- (Seq(tip), List(tip)).tailRecM[F, Seq[A]] {
                    case (acc, blocksToProceed) =>
                      blocksToProceed match {
                        case Nil =>
                          acc.asRight[(Seq[A], List[A])].pure[F]
                        case blocks =>
                          for {
                            nextBlocks <- blocks
                                           .traverse(
                                             b =>
                                               dag
                                                 .parents(b)
                                                 .map(_.getOrElse(Set.empty))
                                           )
                                           .map(_.flatten.distinct)
                            // stop when base block is met
                            next = if (nextBlocks.contains(base)) List.empty else nextBlocks
                          } yield (acc ++ next, next).asLeft[Seq[A]]
                      }
                  }
      // blocks from tipBranch connected to base
      r <- (LinearSeq.empty[A], List(base))
            .tailRecM[F, LinearSeq[A]] {
              case (acc, blocksToProceed) =>
                blocksToProceed match {
                  case Nil =>
                    acc.asRight[(LinearSeq[A], List[A])].pure[F]
                  case blocks =>
                    for {
                      children <- blocks
                                   .traverse(
                                     b =>
                                       dag
                                         .children(b)
                                         .map(_.getOrElse(Seq.empty))
                                   )
                                   .map(_.flatten.distinct)
                      childrenFromTipSet = children.filter(tipBranch.contains)
                    } yield (acc ++ childrenFromTipSet, childrenFromTipSet)
                      .asLeft[LinearSeq[A]]
                }
            }
            // reverse result branch, so first element is tip and last is base
            .map(_.reverse)
      _ = assert(r.nonEmpty)
    } yield r

  /**
    * @return Seq of branches, sorted by size starting with the biggest one.
    *         Each branch is sorted from tip to base.
    */
  def computeSortedDistinctBranches[F[_]: Concurrent, A](
      tips: Seq[A],
      base: A,
      dag: DagReader[F, A]
  )(implicit ord: Ordering[A]): F[Seq[LinearSeq[A]]] =
    for {
      // compute branches in parallel, sort according to size primarily and tip secondary, in descendant order
      sortedBranches <- fs2.Stream
                         .emits(tips.map { top =>
                           fs2.Stream.eval(computeTipToBaseBranch(top, base, dag))
                         })
                         .parJoinUnbounded
                         .compile
                         .toList
                         .map(_.sortBy(b => (b.size, b.head)).reverse)
      // make branches non intersecting
      (_, nonIntersectingBranches) = sortedBranches
        .foldLeft(
          // initial list of branches and result list of non intersecting branches
          (sortedBranches, Seq.empty[LinearSeq[A]])
        ) {
          case ((biggestBranch :: remainder, result), _) => {
            // remove from remainder branches blocks that biggestBranch contains
            val filteredRemainder = remainder.map { b =>
              (b.filterNot(biggestBranch.contains))
            }
            (filteredRemainder, result :+ biggestBranch)
          }
        }
    } yield nonIntersectingBranches
}
