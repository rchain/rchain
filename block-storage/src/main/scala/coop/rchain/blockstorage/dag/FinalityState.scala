package coop.rchain.blockstorage.dag
import cats.Show
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.blockstorage.casper.ConflictsResolver.ConflictResolution
import coop.rchain.blockstorage.casper.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagStorage.DagFringe
import coop.rchain.blockstorage.dag.DeployChainSetCasper.BlockMetadataDag
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.{DeployChain, Justification}
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.models.syntax._
import coop.rchain.pcasper.finalization.{isSupermajority, Fringe}
import coop.rchain.pcasper.finalization.`lazy`.{DagData, LazyFinal, LazyFinalizer, LazyPartitioner}
import coop.rchain.shared.{Base16, Log}

final case class FinalityState[M, S](
    fringeFinal: Fringe[M, S],
    fringeProvisional: Fringe[M, S],
    partitionsProvisioned: Map[Set[S], Set[M]]
)

object FinalityState {

  implicit val showMeta = new Show[BlockMetadata] {
    def show(meta: BlockMetadata): String =
      Base16.encode(meta.blockHash.toByteArray) + s" #${meta.seqNum}"
  }

  /** Update finalization of the network. */
  def updateFinalFringe[F[_]: Log: Concurrent](
      viewSender: Validator,
      view: List[Justification],
      dag: BlockDagRepresentation[F],
      latestFringe: DagFringe,
      bonds: Map[Validator, Long],
      mergeF: (
          StateHash,
          Set[(BlockMetadata, Set[BlockMetadata])]
      ) => F[(ConflictResolution[DeployChain], StateHash)],
      persistNewFringeF: (ConflictResolution[DeployChain], DagFringe) => F[Unit]
  ): F[Unit] = {

    val latestIdx = latestFringe.num
    val lfs       = latestFringe.state

    val witF = (m: BlockMetadata) =>
      dag
        .witnesses(m.blockHash)
        .flatMap(
          _.traverse(
            _.toList
              .traverse { h =>
                dag.lookupUnsafe(h).map(m => m.sender -> m)
              }
              .map(_.toMap)
          )
        )
        .map(_.getOrElse(Map()))
    val jsF = (m: BlockMetadata) =>
      m.justifications
        .map { case Justification(_, latestBlockHash) => latestBlockHash }
        .traverse(
          h => dag.lookupUnsafe(h).map(m => m.sender -> m)
        )
        .map(_.toMap)

    for {
      _ <- Log[F].info(s"Updating finalization state, latestFringe idx $latestIdx")
      latestFringeMeta <- latestFringe.finalizationFringe.toList
                           .traverse {
                             case (s, hs) =>
                               hs.toList
                                 .traverse(h => dag.lookupUnsafe(h))
                                 .map(v => (s, v.toSet))
                           }
      latestMessages <- dag.latestMessages
      recordFinal = (newFinal: Fringe[BlockMetadata, Validator]) =>
        for {
          curFinalFringe <- latestFringeMeta.toMap.mapValues(_.head).pure[F]
          newFinalFringe = curFinalFringe ++ newFinal
          toRecord <- BlockMetadataDag(dag)
                       .finBody(curFinalFringe, newFinalFringe)
                       .map {
                         case (ff, extraMerge) =>
                           List(
                             (
                               ff.toList.map { case (s, v) => (s, Set(v)) },
                               extraMerge
                             )
                           )
                       }
          _ <- ((latestFringeMeta, List.empty[BlockMetadata]) +: toRecord)
                .zip(toRecord)
                .foldLeftM[F, (StateHash, Long)]((lfs, latestIdx + 1)) {
                  case (
                      (prevLfs, nextIdx),
                      ((curFringe, _), (nextFringe, extraMerge))
                      ) =>
                    val curFringeBlocks = curFringe.flatMap(_._2).map(_.blockHash)
                    val conflictSet = nextFringe
                      .filter {
                        case (_, meta) =>
                          (curFringeBlocks.toSet intersect meta.map(_.blockHash)).isEmpty
                      }
                      .flatMap(_._2)
                      .toSet ++ extraMerge
                    for {
                      v <- conflictSet.toList.traverse { m =>
                            val mFringe = dag.finalizationFringes
                              .find {
                                case (_, DagFringe(_, _, num)) => num == m.baseFringeNum
                              }
                              .map(_._2.finalizationFringe)

                            for {
                              _ <- Log[F].info(
                                    s"Merging ${m.show}, baseFF ${m.baseFringeNum}"
                                  )
                              stoppers <- mFringe
                                           .map(_.flatMap(_._2).toSet)
                                           .liftTo(
                                             new Exception(
                                               "no FF for message in DB when merging"
                                             )
                                           )
                              finSet <- curFringe
                                         .map(_._2)
                                         .traverse { m =>
                                           val stream = fs2.Stream
                                             .emits(m.toList)
                                             .covary[F] ++ BlockMetadataDag(dag)
                                             .selfJustificationChain(m.head)
                                           stream
                                             .takeWhile(
                                               m => !stoppers.contains(m.blockHash)
                                             )
                                             .compile
                                             .toList
                                         }
                                         .map(_.flatten)
                            } yield (m, finSet.toSet)
                          }
                      r        <- mergeF(prevLfs, v.toSet)
                      (cr, sh) = r
                      f = DagFringe(
                        nextFringe.map { case (s, m) => (s, m.map(_.blockHash)) },
                        sh,
                        nextIdx
                      )
                      _ <- Log[F].info(
                            s"FF advanced to ${nextFringe
                              .flatMap(_._2.map(_.seqNum))
                              .mkString(";")} (${f.state.toHexString} #${nextIdx})"
                          )
                      _ <- persistNewFringeF(cr, f)
                    } yield (sh, nextIdx + 1)

                }
        } yield ()

      genesis       <- dag.genesis.flatMap(dag.lookupUnsafe)
      genesisView   = bonds.mapValues(_ => genesis)
      jfsAllAcrossF = jsF(_: BlockMetadata).map(genesisView ++ _)
      view <- view
               .map(m => (m.validator, m.latestBlockHash))
               .traverse {
                 case (s, m) => dag.lookupUnsafe(m).map(s -> _)
               }
               .map(genesisView ++ _)

      dg = new DagData[F, BlockMetadata, Validator] {
        override def witnessesF: BlockMetadata => F[Map[Validator, BlockMetadata]] = witF

        override def justificationsF: BlockMetadata => F[Map[Validator, BlockMetadata]] =
          jfsAllAcrossF

        override def seqNum: BlockMetadata => Long = _.seqNum

        override def sender: BlockMetadata => Validator = _.sender
      }

      _ = LazyFinalizer(viewSender, view, bonds, dg).computeFinal(
        List(LazyFinal(latestFringeMeta.toMap.mapValues(_.head), Map()))
      )

//            .flatMap(_.traverse(recordFinal))
    } yield ()
  }
//
//  /** Compute provisional fringe to be associated with the message. */
//  def computeMessageFinal[F[_]: Concurrent](m: BlockMessage, baseDag: BlockDagRepresentation[F])(
//      hash: BlockHash,
//      postStatehash: StateHash,
//      justifications: List[Justification],
//      parentHashes: List[BlockHash],
//      bondz: List[Bond],
//      allFringes: SortedMap[Long, DagFringe],
//      mergeFOpt: Option[
//        (
//            StateHash,
//            Set[(BlockMetadata, Set[BlockMetadata])] // block to merge + finalized scope unseen from this block
//        ) => F[(ConflictResolution[DeployChain], StateHash)]
//      ],
//      persistNewFringeF: (ConflictResolution[DeployChain], DagFringe) => F[Unit]
//  )(implicit log: Log[F]): F[Fringe[BlockHash, Validator]] = {
//    val bonds = bondz.map {
//      case Bond(validator, stake) => validator -> stake
//    }.toMap
//    allFringes.lastOption match {
//      // fringe exists - try update
//      case Some((latestIdx, DagFringe(latestFringe, lfs, _))) =>
//        for {
//          _ <- log.info(s"latestFringe idx $latestIdx")
//          latestFringeMeta <- latestFringe.toList
//                               .traverse {
//                                 case (s, hs) =>
//                                   hs.toList
//                                     .traverse(h => baseDag.lookupUnsafe(h))
//                                     .map(v => (s, v.toSet))
//                               }
//          js <- justifications.traverse {
//                 case Justification(validator, latestBlockHash) =>
//                   baseDag.lookupUnsafe(latestBlockHash).map(validator -> _)
//               }
//          reconciler = new LazyReconciler[F, BlockMetadata, Validator](_.seqNum)
//          witF = (m: BlockMetadata) =>
//            baseDag
//              .witnesses(m.blockHash)
//              .flatMap(
//                _.traverse(
//                  _.toList
//                    .traverse { h =>
//                      baseDag.lookupUnsafe(h).map(m => m.sender -> m)
//                    }
//                    .map(_.toMap)
//                )
//              )
//              .map(_.getOrElse(Map()))
//          jsF = (m: BlockMetadata) =>
//            m.justifications
//              .map { case Justification(_, latestBlockHash) => latestBlockHash }
//              .traverse(
//                h => baseDag.lookupUnsafe(h).map(m => m.sender -> m)
//              )
//              .map(_.toMap)
//          finalF = (m: BlockMetadata) =>
//            m.finalView.toList
//              .traverse {
//                case (s, hash) => baseDag.lookupUnsafe(hash).map(s -> _)
//              }
//              .map(_.toMap)
//          parents <- parentHashes.traverse(baseDag.lookupUnsafe)
//          r <- PCasper
//                .computeFinalityView[F, BlockMetadata, Validator](
//                  js.toMap,
//                  parents,
//                  reconciler,
//                  bonds,
//                  witF,
//                  jsF
//                )(
//                  finalF,
//                  _.seqNum,
//                  _.sender
//                )
//          // TODO record provisionally finalizations into some state. This should be used
//          //  when advancing real finalization to merge bulk of messages in partition as a one item
//          (newFringe, newFinal, _) = r
//          newIdx                   = latestIdx + 1
//          updateFinalFringes = for {
//            _ <- log.info(
//                  s"Finalization advancement detected: #${latestIdx} + " +
//                    s"$newFinal -> #${newIdx}. Provisional fringe is ${newFringe.values}."
//                )
//            curFinalFringe <- latestFringeMeta.toMap.mapValues(_.head).pure[F]
//            newFinalFringe = curFinalFringe ++ newFinal.flatMap { v =>
//              v.map(m => m.sender -> m)
//            }
//            toRecord <- BlockMetadataDag(baseDag)
//                         .finBody(curFinalFringe, newFinalFringe)
//                         .map {
//                           case (ff, extraMerge) =>
//                             List(
//                               (
//                                 ff.toList.map { case (s, v) => (s, Set(v)) },
//                                 extraMerge
//                               )
//                             )
//                         }
//
//            _ <- mergeFOpt.traverse { merge =>
//                  ((latestFringeMeta, List.empty[BlockMetadata]) +: toRecord)
//                    .zip(toRecord)
//                    .foldLeftM[F, (StateHash, Long)]((lfs, latestIdx + 1)) {
//                      case (
//                          (prevLfs, nextIdx),
//                          ((curFringe, _), (nextFringe, extraMerge))
//                          ) =>
//                        val curFringeBlocks = curFringe.flatMap(_._2).map(_.blockHash)
//                        val conflictSet = nextFringe
//                          .filter {
//                            case (_, meta) =>
//                              (curFringeBlocks.toSet intersect meta.map(_.blockHash)).isEmpty
//                          }
//                          .flatMap(_._2)
//                          .toSet ++ extraMerge
//                        for {
//                          v <- conflictSet.toList.traverse { m =>
//                                val mFringe = allFringes
//                                  .find {
//                                    case (_, DagFringe(_, _, num)) => num == m.baseFringeNum
//                                  }
//                                  .map(_._2.finalizationFringe)
//
//                                for {
//                                  _ <- log.info(
//                                        s"Merging ${m.show}, baseFF ${m.baseFringeNum}"
//                                      )
//                                  stoppers <- mFringe
//                                               .map(_.flatMap(_._2).toSet)
//                                               .liftTo(
//                                                 new Exception(
//                                                   "no FF for message in DB when merging"
//                                                 )
//                                               )
//                                  finSet <- curFringe
//                                             .map(_._2)
//                                             .traverse { m =>
//                                               val stream = fs2.Stream
//                                                 .emits(m.toList)
//                                                 .covary[F] ++ BlockMetadataDag(baseDag)
//                                                 .selfJustificationChain(m.head)
//                                               stream
//                                                 .takeWhile(
//                                                   m => !stoppers.contains(m.blockHash)
//                                                 )
//                                                 .compile
//                                                 .toList
//                                             }
//                                             .map(_.flatten)
//                                } yield (m, finSet.toSet)
//                              }
//                          r        <- merge(prevLfs, v.toSet)
//                          (cr, sh) = r
//                          f = DagFringe(
//                            nextFringe.map { case (s, m) => (s, m.map(_.blockHash)) },
//                            sh,
//                            nextIdx
//                          )
//                          _ <- Log[F].info(
//                                s"FF advanced to ${nextFringe
//                                  .flatMap(_._2.map(_.seqNum))
//                                  .mkString(";")} (${f.state.toHexString} #${nextIdx})"
//                              )
//                          _ <- persistNewFringeF(cr, f)
//                        } yield (sh, nextIdx + 1)
//                    }
//                }
//          } yield ()
//
//          _ <- updateFinalFringes.whenA(
//                newFinal.nonEmpty && (newFinal.flatten
//                  .map(_.blockHash) diff latestFringe.map(_._2.head)).nonEmpty
//              )
//
//        } yield newFringe.mapValues(_.blockHash)
//      // does not exist - record block as a fringe
//      case None => {
//        val f = DagFringe(
//          bonds.keySet.map(v => (v, Set(hash))).toList,
//          postStatehash,
//          0L
//        )
//        log.info(s"latestFringesStore is empty. Inserting block as a fringe.") >>
//          persistNewFringeF(ConflictResolution(Set(), Set()), f)
//            .as(bonds.mapValues(v => hash))
//      }
//    }
//  }
}
