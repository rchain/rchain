package coop.rchain.dag

import cats.implicits._
import cats.{Eval, Monad}
import coop.rchain.shared.StreamT

import scala.collection.immutable.{HashSet, Queue}

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
}
