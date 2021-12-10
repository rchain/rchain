package coop.rchain.blockstorage.casper.syntax

import cats.Show
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.casper.Casper._
import coop.rchain.blockstorage.casper._
import coop.rchain.blockstorage.casper.syntax.all._

trait CasperSyntax {
  implicit final def casperSyntax[F[_], M, S](c: Casper[F, M, S]): CasperOps[F, M, S] =
    new CasperOps[F, M, S](c)
}

final class CasperOps[F[_], M, S](val c: Casper[F, M, S]) extends AnyVal {
  import c._

  /**
    * Update finalization.
    * @return List of new finalization fringes found + conflict scope that should be merged into current finalized state.
    */
  def finalise(
      latestMessages: List[(S, M)],
      dag: DependencyGraph[F, M, S],
      safetyOracle: SafetyOracle2[F, M, S],
      currLatestFringe: FinalizationFringe[S, M]
  )(
      implicit sync: Sync[F],
      ordering: Ordering[M],
      show: Show[M]
  ): F[Vector[FinalizationFringe[S, M]]] = {
    // Slice is complete when all messages in a slice have enough stake (are below supermajority fringe)
    def fringeIsComplete(
        fringe: FinalizationFringe[S, M],
        supermajorityFringe: FinalizationFringe[S, M]
    ): Boolean = {
      val (g, ng)    = supermajorityFringe.partition { case (_, d) => dag.seqNum(d.head) == 0 }
      val genesisOpt = g.headOption.map(_._2.head)
      val gRecordsOpt = genesisOpt.map(
        genesis => safetyOracle.bondsMap(genesis).keySet.map((_, Set(genesis))).toMap
      )
      val ngM      = ng.toMap
      val toAddOpt = gRecordsOpt.map(_ filterKeys (!ngM.keySet.contains(_)))

      val smfringe = toAddOpt.map(ngM ++ _).getOrElse(ngM)

      val supermajoritySeqNums = smfringe.mapValues(_.map(dag.seqNum).max)

      val candidate = fringe.map { case (s, m) => (s, m.map(dag.seqNum).max) }.toMap
      !candidate.exists {
        case (s, seqNum) => supermajoritySeqNums(s) < seqNum
      }
    }

    def nextFringe(ff: FinalizationFringe[S, M]): F[FinalizationFringe[S, M]] =
      ff.toList
        .traverse {
          case (s, msgs) =>
            for {
              newMsgs <- msgs.toList
                          .flatTraverse(
                            m => dag.children(m).map(_.filter(dag.sender(_) == s))
                          )
                          // this is required for genesis case
                          .map(
                            _.toSet
                              .groupBy(dag.seqNum)
                              .toList
                              .sortBy(_._1)
                              .map(_._2)
                              .headOption
                              .getOrElse(Set())
                          )
              // no messages that have justifications above current FF allowed
              filtered <- newMsgs.toList.filterA { m =>
                           for {
                             js <- dag.justifications(m)
                             tooNew = js.exists { j =>
                               dag.seqNum(j) > ff
                                 .find { case (s, _) => s == dag.sender(j) }
                                 .get
                                 .map(_.map(dag.seqNum).min)
                                 ._2
                             }
                           } yield !tooNew
                         }
              r = if (filtered.nonEmpty) filtered.toSet else msgs
            } yield (s, r)
        }

    Sync[F].handleErrorWith(
      for {
        // find supermajority fringe
        r                                    <- messageScope(latestMessages, dag, safetyOracle)
        MessageScope(supermajorityFringe, _) = r
        // traverse up from current finalization fringe
        newFringes <- fs2.Stream
                       .unfoldEval(currLatestFringe) {
                         cur =>
                           nextFringe(cur).map {
                             next =>
                               // TODO handle eqs
                               assert(
                                 next.map(_._2).forall(_.size == 1),
                                 s"multiple messages from same sender in ffringe. Should be fine if equivocations are handled but they are not now. " +
                                   s"${cur.map(_._2.map(_.show))} -> ${next.map(_._2.map(_.show))}"
                               )
                               (cur != next &&
                                 fringeIsComplete(next, supermajorityFringe))
                                 .guard[Option]
                                 .as(next, next)
                           }
                       }
                       .compile
                       .to(Vector)
      } yield newFringes
    ) {
      case NoFinalizationFringe => Vector.empty[FinalizationFringe[S, M]].pure[F]
    }
  }

  def messageScope(
      lms: List[(S, M)],
      dag: DependencyGraph[F, M, S],
      safetyOracle: SafetyOracle2[F, M, S]
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[MessageScope[S, M]] = {
    import dag._
    val latestMessages = lms.map(_._2).toSet

    val allSenders    = latestMessages.map(sender)
    val latestSeqNums = latestMessages.map(m => sender(m) -> seqNum(m)).toMap
    val visitsInit    = Map.empty[S, Set[M]] // Accumulator for messages visited while pulling the stream.
    val fringeInit    = Map.empty[S, M] // Accumulator for finalization fringe

    safetyOracle
      .faultTolerances(latestMessages, dag)
      // Accumulate fringe and messages visited
      .scan((fringeInit, visitsInit)) {
        case ((fringeAcc, visitsAcc), level) =>
          val finalized = level.collect { case (m, ft) if ft > faultToleranceThreshold => m }
          val newFringeAcc = finalized.foldLeft(fringeAcc) {
            case (acc, m) =>
              val s                      = sender(m)
              val shouldRecordFringeItem = !acc.get(s).exists(seqNum(_) >= seqNum(m))
              if (shouldRecordFringeItem) acc.updated(s, m) else acc
          }
          val newVisitsAcc = level.foldLeft(visitsAcc) {
            case (acc, (m, _)) =>
              val s = sender(m)
              acc.updated(s, acc.get(s).map(_ + m).getOrElse(Set(m)))
          }
          (newFringeAcc, newVisitsAcc)
      }
      // Pull stream until fringe is completed and it is certain that all items are highest.
      // Once N items are pulled from stream, which is N justifications, no one can reference messages higher then
      // seqNum(tip) - N. Therefore no results higher then already found.
      .zipWithIndex
      .find {
        case ((fringeAcc, _), idx) =>
          val fringeComplete = fringeAcc.keySet == allSenders
          val fringeIsHighest =
            fringeAcc.valuesIterator.forall(m => idx - 1 > latestSeqNums(sender(m)) - seqNum(m))
          fringeComplete && fringeIsHighest
      }
      // Clear non finalized set accumulator from messages below the fringe
      .map {
        case ((fringe, visited), _) =>
          val nonFinalizedSet = visited.flatMap {
            case (sender, messages) => messages.filter(nf => seqNum(nf) >= seqNum(fringe(sender)))
          }
          MessageScope(
            fringe.values.toList.map(m => (dag.sender(m), Set(m))),
            ConflictScope(nonFinalizedSet.toSet)
          )
      }
      .compile
      .last
      // Throw exception if no fringe found
      .flatMap(_.liftTo(NoFinalizationFringe))
  }
}
