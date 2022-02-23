package coop.rchain.blockstorage.casper.syntax

import cats.Show
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.casper.DependencyGraph.{
  zipStreamList,
  CommonMessage,
  NoCommonMessage
}
import coop.rchain.blockstorage.casper.SafetyOracle2.Agreement
import coop.rchain.blockstorage.casper._
import coop.rchain.shared.Log
import fs2.{Chunk, Stream}

import scala.collection.mutable

trait DependencyGraphSyntax {
  implicit final def DependencyGraphSyntax[F[_], M, S](
      c: DependencyGraph[F, M, S]
  ): DependencyGraphOps[F, M, S] =
    new DependencyGraphOps[F, M, S](c)
}

final class DependencyGraphOps[F[_], M, S](val dg: DependencyGraph[F, M, S]) extends AnyVal {
  import dg._

  /** Stream of self descendants. */
  def selfDescendantsChain(message: M)(implicit a: Sync[F]): Stream[F, M] = {
    val s = sender(message)
    Stream.unfoldEval(message)(children(_).map(_.find(sender(_) == s).map(v => (v, v))))
  }

  /** Stream of self descendants. */
  def descendantsChain(message: M, sender: S)(implicit a: Sync[F]): Stream[F, M] =
    Stream.unfoldEval(message)(children(_).map(_.find(dg.sender(_) == sender).map(v => (v, v))))

//  def fringeCandidate(curFringe: Map[S, M])(implicit a: Sync[F]): F[Map[S, Option[M]]] =
//    curFringe.toList
//      .traverse {
//        case (s, m) => dg.children(m).map(_.find(sender(_) == s)).map(v => (s, v))
//      }
//      .map(_.toMap)

  def finBody(
      curFringe: Map[S, M],
      newFringe: Map[S, M]
  )(implicit c: Concurrent[F], log: Log[F], show: Show[M]): F[(Map[S, M], List[M])] =
    for {
      r <- newFringe.some.map(curFringe ++ _).traverse { fringe =>
            for {
              childrenMaps <- fringe.values.toList
                               .traverse { m =>
                                 dg.justifications(m)
                                   .map(_.map(j => (dg.sender(j), (j, dg.seqNum(j)))).toMap)
                               }
              highestParents = childrenMaps
                .reduce { (l, r) =>
                  l.map {
                    case (s, (m, sn)) =>
                      if (r.get(s).exists(_._2 > sn)) (s, r(s)) else (s, (m, sn))
                  }
                }
              toMove = highestParents.filter {
                case (s, (_, sn)) => sn > dg.seqNum(fringe(s))
              }
              extraMsg <- toMove.toList.flatTraverse {
                           case (s, (m, _)) =>
                             selfJustificationChain(m)
                               .takeWhile(
                                 seqNum(_) > dg.seqNum(fringe(s))
                               )
                               .compile
                               .toList
                         }
            } yield (fringe ++ toMove.mapValues(_._1), extraMsg)
          }
      _ <- log
            .info(
              s"Found new FF ${r.get._1.values.map(_.show)}, extra messages finalized: ${r.get._2.size}."
            )
            .whenA(r.isDefined)
    } yield r.get // update current fringe with new messages

//  def nextFF(
//      latestMessages: Map[S, M],
//      curFringe: Map[S, M],
//      fullBondsMap: Map[S, Long],
//      computedCoveringsMap: mutable.TreeMap[S, Set[M]]
//  )(
//      implicit c: Concurrent[F],
//      log: Log[F],
//      show: Show[M]
//      //shows: Show[S]
//  ): F[Option[(Map[S, M], List[M])]] = {
//    val finalizer = Finalizer(latestMessages, curFringe)
//    for {
//      fringe <- finalizer.run(
//                 dg.witnesses(_)
//                   .map(_.map(w => dg.sender(w) -> w).toMap)
//                   .flatTap(v => log.info(s"${v.size}")),
//                 dg.justifications(_).map(_.map(j => dg.sender(j) -> j).toMap)
//               )(dg.seqNum, dg.sender)
////      fringeCandidate <- fringeCandidate(curFringe)
//      _ <- log.info(s"new FF candidate: ${fringe.map { case (_, m) => m.show }}")
////      fringe = fringeCandidate.collect { case (s, Some(v)) => (s, v) }
//      // fringes that are smaller then supermajority should not be considered, as they cannot match the criteria so no reason to spend compute.
//      safe = fringe.nonEmpty && !Fringe.isFinal(fringe)(fullBondsMap)
////      safe <- (fullBondsMap.keySet == fringe.keySet)
////               .pure[F] //||^ SimpleProtocol1.isPartition(fringeCandidate, dg.children, dg.sender)
//      //      _ <- log
//      //            .info(s"Partition detected")
//      //            .whenA(safe && fringeCandidate.valuesIterator.contains(none[M]))
//      newFringe <- if (safe) {
//                    fringe.some.pure //combine(curFringe, fringe)(dg.seqNum).some.pure
////                    val bondsMap = fullBondsMap.filterKeys(fringe.keySet.contains)
////                    SimpleProtocol1
////                      .run(fringe, bondsMap, dg.children, dg.sender)
////                      .map(
////                        (if (_) fringe.some else none[Map[S, M]])
////                      )
//                  } else none[Map[S, M]].pure[F]
//
//      r <- newFringe.map(curFringe ++ _).traverse { fringe =>
//            for {
//              childrenMaps <- fringe.values.toList
//                               .traverse { m =>
//                                 dg.justifications(m)
//                                   .map(_.map(j => (dg.sender(j), (j, dg.seqNum(j)))).toMap)
//                               }
//              highestParents = childrenMaps
//                .reduce { (l, r) =>
//                  l.map {
//                    case (s, (m, sn)) =>
//                      if (r.get(s).exists(_._2 > sn)) (s, r(s)) else (s, (m, sn))
//                  }
//                }
//              toMove = highestParents.filter {
//                case (s, (_, sn)) => sn > dg.seqNum(fringe(s))
//              }
//              extraMsg <- toMove.toList.flatTraverse {
//                           case (s, (m, _)) =>
//                             selfJustificationChain(m)
//                               .takeWhile(
//                                 seqNum(_) > dg.seqNum(fringe(s))
//                               )
//                               .compile
//                               .toList
//                         }
//            } yield (fringe ++ toMove.mapValues(_._1), extraMsg)
//          }
//      _ <- log
//            .info(
//              s"Found new FF ${r.get._1.values.map(_.show)}, extra messages finalized: ${r.get._2.size}."
//            )
//            .whenA(r.isDefined)
//    } yield r // update current fringe with new messages
//  }

//  def nextFF(
//      latestMessages: Map[S, M],
//      curFringe: Map[S, M],
//      fullBondsMap: Map[S, Long],
//      computedCoveringsMap: mutable.TreeMap[S, Set[M]]
//  )(
//      implicit c: Concurrent[F],
//      log: Log[F],
//      show: Show[M]
//      //shows: Show[S]
//  ): F[Option[(Map[S, M], List[M])]] =
//    for {
//      fringeCandidate <- fringeCandidate(curFringe)
//      _               <- log.info(s"new FF candidate: ${fringeCandidate.map { case (_, m) => m.show }}")
//      fringe          = fringeCandidate.collect { case (s, Some(v)) => (s, v) }
//      // fringes that are smaller then supermajority should not be considered, as they cannot match the criteria so no reason to spend compute.
//      safe <- (fullBondsMap.keySet == fringe.keySet)
//               .pure[F] //||^ SimpleProtocol1.isPartition(fringeCandidate, dg.children, dg.sender)
////      _ <- log
////            .info(s"Partition detected")
////            .whenA(safe && fringeCandidate.valuesIterator.contains(none[M]))
//      newFringe <- if (safe) {
//                    val bondsMap = fullBondsMap.filterKeys(fringe.keySet.contains)
//                    SimpleProtocol1
//                      .run(fringe, bondsMap, dg.children, dg.sender)
//                      .map(
//                        (if (_) fringe.some else none[Map[S, M]])
//                      )
//                  } else none[Map[S, M]].pure[F]
//
//      r <- newFringe.map(curFringe ++ _).traverse { fringe =>
//            for {
//              childrenMaps <- fringe.values.toList
//                               .traverse { m =>
//                                 dg.justifications(m)
//                                   .map(_.map(j => (dg.sender(j), (j, dg.seqNum(j)))).toMap)
//                               }
//              highestParents = childrenMaps
//                .reduce { (l, r) =>
//                  l.map {
//                    case (s, (m, sn)) =>
//                      if (r.get(s).exists(_._2 > sn)) (s, r(s)) else (s, (m, sn))
//                  }
//                }
//              toMove = highestParents.filter {
//                case (s, (_, sn)) => sn > dg.seqNum(fringe(s))
//              }
//              extraMsg <- toMove.toList.flatTraverse {
//                           case (s, (m, _)) =>
//                             selfJustificationChain(m)
//                               .takeWhile(
//                                 seqNum(_) > dg.seqNum(fringe(s))
//                               )
//                               .compile
//                               .toList
//                         }
//            } yield (fringe ++ toMove.mapValues(_._1), extraMsg)
//          }
//      _ <- log
//            .info(
//              s"Found new FF ${r.get._1.values.map(_.show)}, extra messages finalized: ${r.get._2.size}."
//            )
//            .whenA(r.isDefined)
//    } yield r // update current fringe with new messages

//  def nextFF(
//      latestMessages: Map[S, M],
//      curFringe: Map[S, M],
//      bondsMap: Map[S, Long],
//      computedCoveringsMap: mutable.TreeMap[S, Set[M]]
//  )(
//      implicit a: Sync[F],
//      log: Log[F],
//      show: Show[M],
//      ordering: Ordering[M]
//  ): F[Option[(Map[S, M], List[M])]] =
//    for {
//      fringeCandidate <- fringeCandidate(curFringe)
//      _               <- log.info(s"new FF candidate: ${fringeCandidate.map { case (_, m) => m.show }}")
//      // fringes that are smaller then supermajority should not be considered, as they cannot match the criteria so no reason to spend compute.
//      fringeStake        = fringeCandidate.values.flatten.map(sender).map(bondsMap).sum
//      totalStake         = bondsMap.values.sum
//      fringeIsSufficient = fringeStake * 3 > totalStake * 2
//      //supermajorityFringe <- supermajorityFringe(latestMessages, bondsMap).map(curFringe ++ _)
//      newFringe <- if (fringeIsSufficient) for {
//                    fringe <- Sync[F].delay(fringeCandidate.collect {
//                               case (s, v) if v.isDefined => (s, v.get)
//                             })
//                    r <- Casper
//                          .nextFinalizationRound(fringe, dg, bondsMap)
//                          .map(_.map(_.map(m => (dg.sender(m), m)).toMap))
//                  } yield r
//                  else none[Map[S, M]].pure[F]
////      newFringe <- if (fringeIsSufficient) for {
////                    fringe <- Sync[F].delay(fringeCandidate.collect {
////                               case (s, v) if v.isDefined => (s, v.get)
////                             })
////                    supermajorityBase = Stream
////                      .emits(supermajorityFringe.toList)
////                      .covary[F]
////                      .evalMap {
////                        case (s, m) =>
////                          Casper
////                            .coveringBase(m, fringe, curFringe, dg, bondsMap, computedCoveringsMap)
////                            .map((s, _))
////                            .flatTap {
////                              case (_, base) =>
////                                new Exception(s"Base computed is not part of target fringe").raiseError
////                                  .whenA((base -- fringe.values).nonEmpty)
////                            }
////                      }
////                      // only coverings with supermajority base are accounted
////                      .filter {
////                        case (_, base) =>
////                          base.toList.map(sender).map(bondsMap).sum * 3 > totalStake * 2
////                      }
////                      .evalTap { case (s, base) => computedCoveringsMap.update(s, base).pure[F] }
////                      .fold((Set.empty[S], fringe.values.toSet)) {
////                        case (acc, (s, base)) => (acc._1 + s, acc._2 intersect base)
////                      }
////                      // output is what is commonly covered by supermajority of coverings
////                      .filter {
////                        case (senders, base) =>
////                          base.nonEmpty && senders.toList.map(bondsMap).sum * 3 > totalStake * 2
////                      }
////                      .map(_._2)
////
////                    // advance fringe if coverings are supermajority
////                    r <- supermajorityBase.compile.last
////                          .map(
////                            _.map(base => base.map(b => (sender(b), b)).toMap.some)
////                              .getOrElse(none[Map[S, M]])
////                          )
////                  } yield r
////                  else none[Map[S, M]].pure[F]
//      r <- newFringe.map(curFringe ++ _).traverse { fringe =>
//            for {
//              childrenMaps <- fringe.values.toList
//                               .traverse { m =>
//                                 dg.justifications(m)
//                                   .map(_.map(j => (dg.sender(j), (j, dg.seqNum(j)))).toMap)
//                               }
//              highestParents = childrenMaps
//                .reduce { (l, r) =>
//                  l.map {
//                    case (s, (m, sn)) =>
//                      if (r.get(s).exists(_._2 > sn)) (s, r(s)) else (s, (m, sn))
//                  }
//                }
//              toMove = highestParents.filter {
//                case (s, (_, sn)) => sn > dg.seqNum(fringe(s))
//              }
//              extraMsg <- toMove.toList.flatTraverse {
//                           case (s, (m, _)) =>
//                             selfJustificationChain(m)
//                               .takeWhile(
//                                 seqNum(_) > dg.seqNum(fringe(s))
//                               )
//                               .compile
//                               .toList
//                         }
//            } yield (fringe ++ toMove.mapValues(_._1), extraMsg)
//          }
//      _ <- log
//            .info(
//              s"Found new FF ${r.get._1.values.map(_.show)}, extra messages finalized: ${r.get._2.size}."
//            )
//            .whenA(r.isDefined)
//    } yield r // update current fringe with new messages

  def supermajorityFringe(
      lms: Map[S, M],
      bondsMap: Map[S, Long]
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[Map[S, M]] = {
    val latestMessages = lms.values.toSet

    val allSenders    = latestMessages.map(sender)
    val latestSeqNums = latestMessages.map(m => sender(m) -> seqNum(m)).toMap
    val visitsInit    = Map.empty[S, Set[M]] // Accumulator for messages visited while pulling the stream.
    val fringeInit    = Map.empty[S, M] // Accumulator for finalization fringe

    val visited = mutable.TreeMap.empty[M, List[M]]
    messagesView(latestMessages)(
      m => justifications(m).map(_ diff visited.getOrElse(m, List()))
    ).mapAccumulate(Map.empty[M, Set[Agreement[M]]]) { (acc, chunk) =>
        val newAcc = chunk.foldLeft(acc) {
          case (lvlAcc, (visitor, targets)) =>
            visited.update(visitor, targets)
            targets.foldLeft(lvlAcc) { (visitorAcc, target) =>
              visitorAcc.updated(
                target,
                visitorAcc.getOrElse(target, Set()) + Agreement(visitor, target)
              )
            }
        }
        val chunkTargets = chunk.toList.flatMap { case (_, targets) => targets }
        val out = newAcc.filterKeys(chunkTargets.contains).map {
          case (target, agreements) =>
            (target, SafetyOracle2.faultTolerance(agreements, (_: M) => bondsMap, sender))
        }
        (newAcc, out)
      }
      .map { case (_, v) => v.toList.sortBy { case (m, _) => m } }
      // Accumulate fringe and messages visited
      .scan((fringeInit, visitsInit)) {
        case ((fringeAcc, visitsAcc), level) =>
          val finalized = level.collect { case (m, ft) if ft > -1 => m }
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
      .takeThrough {
        case ((fringeAcc, _), idx) =>
          val fringeComplete = fringeAcc.keySet == allSenders
          val fringeIsHighest =
            fringeAcc.valuesIterator.forall(m => idx - 1 > latestSeqNums(sender(m)) - seqNum(m))
          !(fringeComplete && fringeIsHighest)
      }
      // Clear non finalized set accumulator from messages below the fringe
      .map { case ((fringe, _), _) => fringe }
      .compile
      .lastOrError
  }
//  def nextFF(
//      curFringe: Map[S, M],
//      bondsMap: Map[S, Long]
//  )(
//      implicit a: Sync[F],
//      log: Log[F],
//      show: Show[M],
//      ordering: Ordering[M]
//  ): F[Option[Map[S, M]]] = {
//    val totalStake = bondsMap.values.sum
//    for {
//      fringeCandidate <- fringeCandidate(curFringe)
//      // fringes that are smaller then supermajority should not be considered, as they cannot match the criteria so no reason to spend compute.
//      fringeStake        = fringeCandidate.values.flatten.map(sender).map(bondsMap).sum
//      fringeIsSufficient = fringeStake * 3 < totalStake * 2
//
//      range = 2L
//      _     <- log.info(s"new FF candidate: ${fringeCandidate.map { case (_, m) => m.show }}")
//      fullFC = fringeCandidate.map {
//        case (s, mOpt) =>
//          require(
//            curFringe.contains(s),
//            "Current fringe does not have sender existing in next candidate"
//          )
//          (s, mOpt.getOrElse(curFringe(s)))
//      }
//      fMsg <- fullFC.toList.traverse {
//               case (s, m) => descendantsChain(m, s).take(range).compile.toList.map((s, _))
//             }
//      stoppers = fullFC.mapValues(seqNum) ++ fMsg
//        .map(_._2)
//        .filter(_.nonEmpty)
//        .map(_.last)
//        .map(m => (sender(m), seqNum(m)))
//        .toMap
//      noAgreementsMap = bondsMap.mapValues(_ => 0L)
//      stakes <- fringeCandidate.toList.traverse {
//                 case (sndr, mOpt) =>
//                   mOpt
//                     .traverse { m =>
//                       val relation = (m: M) =>
//                         children(m).map(_.filter { c =>
//                           val cSender = sender(c)
//                           val cSeqNum = seqNum(c)
//                           cSeqNum <= stoppers(cSender)
//                         })
//                       (Stream(m) ++ uniqueRelatives(m, 50L)(relation))
//                         .fold(noAgreementsMap) {
//                           case (acc, descendant) =>
//                             val agreer = sender(descendant)
//                             acc.updated(agreer, bondsMap(agreer))
//                         }
//                         .compile
//                         .lastOrError
//                     }
//                     .map(_.getOrElse(noAgreementsMap))
//                     .map((sndr, _))
//               }
//      (certainMsgs, uncertainMsgs) = stakes
//        .partition { case (_, agreedStake) => agreedStake.values.sum * 3 > totalStake * 2 }
//
//      uncertainStake = fMsg.filter(_._2.size < range).map { case (s, _) => (s, bondsMap(s)) }.toMap
//
//      // uncertain stake cannot change the fringe
//      shouldFinalize = certainMsgs.nonEmpty &&
//        !uncertainMsgs.exists {
//          case (s, agreedStake) =>
//            //self stake cannot be uncertain
//            (agreedStake ++ uncertainStake.filterKeys(_ != s)).values.sum * 3 > totalStake * 2
//        }
//
//      newFringe = uncertainMsgs.foldLeft(fullFC) {
//        case (acc, (s, _)) => acc.updated(s, curFringe(s))
//      }
//      _ <- log.info(s"Found new FF ${newFringe.values.map(_.show)}").whenA(shouldFinalize)
//    } yield shouldFinalize.guard[Option].as(newFringe)
//  }

  /**
    * @return Stream containing message + ancestors of the message (justifications), topologically sorted.
    *         Messages of the same topological order are sorted by identity.
    *         Can contain duplicates, as the same message can be seen through multiple paths.
    *         Not flattened to keep the notion of distance.
    */
  def toposortView(
      message: M
  )(relation: M => F[List[M]])(implicit a: Sync[F], ordering: Ordering[M]): Stream[F, List[M]] =
    Stream
      .unfoldLoopEval(List(message)) { lvl =>
        lvl
          .flatTraverse(s => relation(s))
          // Sort output to keep function pure
          .map(_.distinct.sorted)
          .map(next => (next, next.nonEmpty.guard[Option].as(next)))
      }

  /**
    * @return Stream containing message + ancestors of the message (justifications), topologically sorted.
    *         Messages of the same topological order are sorted by identity.
    *         Can contain duplicates, as the same message can be seen through multiple paths.
    *         Not flattened to keep the notion of distance.
    */
  def messageView(
      message: M
  )(relation: M => F[List[M]])(implicit a: Sync[F], ordering: Ordering[M]): Stream[F, List[M]] =
    toposortView(message)(relation)

  /**
    * @return Combined stream of views of the messages.
    *         Per message streams are zipped, to preserve topological sorting.
    */
  def messagesView(
      messages: Set[M]
  )(
      relation: M => F[List[M]]
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, Chunk[(M, List[M])]] = {
    // Sort output to keep function pure
    val sorted = messages.toList.sorted
    zipStreamList(sorted.map(m => messageView(m)(relation).map((m, _)).zipWithIndex))
      .groupAdjacentBy { case (_, idx) => idx }
      .map { case (_, chunk) => chunk.map { case (v, _) => v } }
  }

  def uniqueRelatives(
      message: M,
      depth: Long = Long.MaxValue
  )(
      relation: M => F[List[M]]
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, M] = {
    val visited = mutable.TreeMap.empty[M, Vector[M]]
    messageView(message)(
      m => relation(m).map(_ diff visited.getOrElse(m, List()))
    ).take(depth).flatMap(Stream.emits)
  }

  def messageDescendants(
      message: M,
      depth: Long = Long.MaxValue
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, M] = {
    val visited = mutable.TreeMap.empty[M, Vector[M]]
    messageView(message)(
      m => children(m).map(_ diff visited.getOrElse(m, List()))
    ).take(depth).flatMap(Stream.emits)
  }

  /**
    * @return Stream of self justifications.
    */
  def selfJustificationChain(message: M)(implicit a: Sync[F]): Stream[F, M] = {
    val s = sender(message)
    Stream.unfoldEval(message)(justifications(_).map(_.find(sender(_) == s).map(v => (v, v))))
  }

  def highestCommonMessage(
      messages: Set[M],
      requirement: M => Boolean = _ => true
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[Option[CommonMessage[M]]] = {
    val latestSeqNums =
      messages.map(m => sender(m) -> seqNum(m)).toMap
    val initConnectionsMap = Map.empty[M, Set[M]]
    messagesView(messages)(dg.parents)
      .flatMap(chunk => Stream.emits(chunk.toList.flatMap { case (s, t) => t.map((s, _)) }))
      .evalMapAccumulate(initConnectionsMap) {
        case (acc, (visitor, target)) =>
          val newVisitors = acc.getOrElse(target, Set.empty[M]) + visitor
          val newAcc      = acc.updated(target, newVisitors)
          // Messages that did not reach target through justifications yet
          val yetNotVisitors = messages -- newVisitors
          // If target has yetNotVisitors in justification - result is reached
          justifications(target)
            .map { js =>
              ((yetNotVisitors -- js).isEmpty && requirement(target))
                .guard[Option]
                .as(CommonMessage(target, js.toSet, newVisitors - target))
            }
            .map(v => (newAcc, v))
      }
      .map { case (_, v) => v }
      .zipWithIndex
      .mapAccumulate(List.empty[CommonMessage[M]]) {
        case (acc, (v, idx)) => (v.map(_ +: acc).getOrElse(acc), idx)
      }
      .mapFilter {
        case (acc, idx) =>
          acc.find {
            case CommonMessage(m, _, _) =>
              idx >= latestSeqNums(sender(m)) - seqNum(m)
          }
      }
      .take(1)
      .compile
      .last
  }

  /**
    * @return Scope required to merge multiple messages.
    */
  def mergeScope(messages: Set[M], requirement: M => Boolean = _ => true)(
      implicit sync: Sync[F],
      ordering: Ordering[M]
  ): F[(CommonMessage[M], Set[M])] =
    highestCommonMessage(messages, requirement)
      .flatMap(_.liftTo[F](NoCommonMessage))
      .flatMap {
        case hcm @ CommonMessage(base, _, unseenPart) =>
          justifications(base)
            .flatMap { baseJustifications =>
              val streams = unseenPart.toList.map { fringeMessage =>
                selfJustificationChain(fringeMessage)
                  .takeWhile(m => !(m == base || baseJustifications.contains(m)))
              }
              Stream
                .emits(streams)
                .covary[F]
                .flatten
                .compile
                .to(Set)
                .map(v => (hcm, v))
            }
      }
}
