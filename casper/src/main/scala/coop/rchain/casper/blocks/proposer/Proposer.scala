package coop.rchain.casper.blocks.proposer

import cats.data.OptionT
import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.blocks.proposer.BlockCreatorResult.created
import coop.rchain.casper.protocol.{BlockMessage, CommUtil}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Validator.Validator
import coop.rchain.sdk.error.FatalError
import coop.rchain.shared.syntax._
import coop.rchain.models.syntax._
import coop.rchain.shared.{Log, Time}

sealed abstract class ProposerResult
object ProposerEmpty                                                         extends ProposerResult
final case class ProposerSuccess(status: ProposeStatus, block: BlockMessage) extends ProposerResult
final case class ProposerFailure(status: ProposeStatus, seqNumber: Long)     extends ProposerResult
final case class ProposerStarted(seqNumber: Long)                            extends ProposerResult

object ProposerResult {
  def empty: ProposerResult = ProposerEmpty
  def success(status: ProposeStatus, block: BlockMessage): ProposerResult =
    ProposerSuccess(status, block)
  def failure(status: ProposeStatus, seqNumber: Long): ProposerResult =
    ProposerFailure(status, seqNumber)
  def started(seqNumber: Long): ProposerResult = ProposerStarted(seqNumber)
}

class Proposer[F[_]: Concurrent: Log: Span](
    getLatestSeqNumber: Validator => F[Long],
    // propose constraint checkers
    checkActiveValidator: ValidatorIdentity => F[Boolean],
    createBlock: ValidatorIdentity => F[BlockCreatorResult],
    validateBlock: BlockMessage => F[ValidBlockProcessing],
    proposeEffect: BlockMessage => F[Unit],
    validator: ValidatorIdentity
) {

  implicit val RuntimeMetricsSource: Source = Metrics.Source(CasperMetricsSource, "proposer")
  // This is the whole logic of propose
  private def doPropose: F[(ProposeResult, Option[BlockMessage])] =
    Span[F].traceI("do-propose") {
      // check if node is allowed to propose a block
      checkActiveValidator(validator).ifM(
        for {
          b <- createBlock(validator)
          r <- b match {
                case NoNewDeploys =>
                  (ProposeResult.failure(NoNewDeploys), none[BlockMessage]).pure[F]
                case Created(b) =>
                  validateBlock(b).flatMap {
                    case Right(v) =>
                      proposeEffect(b) >>
                        (ProposeResult.success(v), b.some).pure[F]
                    case Left(v) =>
                      Concurrent[F].raiseError[(ProposeResult, Option[BlockMessage])](
                        new Exception(
                          s"Validation of self created block failed with reason: $v, cancelling propose."
                        )
                      )
                  }
              }
        } yield r,
        (ProposeResult.failure(NotBonded), none[BlockMessage]).pure[F]
      )
    }

  def propose(
      isAsync: Boolean,
      proposeIdDef: Deferred[F, ProposerResult]
  ): F[(ProposeResult, Option[BlockMessage])] = {
    val valBytes = ByteString.copyFrom(validator.publicKey.bytes)
    for {
      validatorSeqNum <- getLatestSeqNumber(valBytes)
      nextSeq         = validatorSeqNum + 1L
      result <- if (isAsync) for {
                 _ <- proposeIdDef.complete(ProposerResult.started(nextSeq))

                 // propose
                 r <- doPropose
               } yield r
               else
                 for {
                   // propose
                   r <- doPropose

                   (result, blockHashOpt) = r
                   proposerResult = blockHashOpt.fold {
                     ProposerResult.failure(result.proposeStatus, nextSeq)
                   } { block =>
                     ProposerResult.success(result.proposeStatus, block)
                   }
                   _ <- proposeIdDef.complete(proposerResult)
                 } yield r

    } yield result
  }
}

object Proposer {
  // format: off
  def apply[F[_]
    /* Execution */   : Concurrent: Timer: Time
    /* Storage */     : BlockStore: BlockDagStorage
    /* Rholang */     : RuntimeManager
    /* Comm */        : CommUtil
    /* Diagnostics */ : Log: Span: Metrics
  ] // format: on
  (
      validatorIdentity: ValidatorIdentity,
      shardId: String,
      minPhloPrice: Long,
      epochLength: Int,
      dummyDeployOpt: Option[(PrivateKey, String)] = None
  ): Proposer[F] = {
    // TODO: refactor proposer to get this from parent pre state
    def getLatestSeqNumber(sender: Validator): F[Long] =
      for {
        dag        <- BlockDagStorage[F].getRepresentation
        latestMsgs = dag.dagMessageState.latestMsgs
        maxSeqNum  = latestMsgs.find(_.sender == sender).map(_.senderSeq)
      } yield maxSeqNum.getOrElse(-1)

    def createBlock(validatorIdentity: ValidatorIdentity): F[BlockCreatorResult] =
//      for {
//        badBlock <- BlockStore[F]
//                     .getUnsafe(
//                       "6fcd3117091491bc656460dc7e2532cce2cef3f255b6885bcc4bca6a92ba00ff".unsafeHexToByteString
//                     )
//        goodBlock <- BlockStore[F]
//                      .getUnsafe(
//                        "a5bb832a145dc1cd0c4e35fab2a36e61ba8c9c360044279a949fee0501edc9a5".unsafeHexToByteString
//                      )
//
//        _ <- Log[F].info(s"Bad block the state from good block")
//        s = goodBlock.state
//
//        b = badBlock.copy(state = s)
//        _ <- Log[F].info(s"Loading ${b.blockHash.show}")
//      } yield created(b)
      for {
        // merge pre state
        preState <- MultiParentCasper.getPreStateForNewBlock
        // misc
        preStateHash      = preState.preStateHash
        creatorsPk        = validatorIdentity.publicKey
        creatorsId        = ByteString.copyFrom(creatorsPk.bytes)
        creatorsLatestOpt = preState.justifications.find(_.sender == creatorsId)
        nextSeqNum        = creatorsLatestOpt.map(_.seqNum + 1).getOrElse(0L)
        nextBlockNum      = preState.justifications.map(_.blockNum).max + 1
        parentHashes      = preState.justifications.map(_.blockHash)
        finalBonds        = preState.fringeBondsMap
        offenders         = preState.justifications.filter(_.validationFailed).map(_.sender)
        // slashing
        preStateBonds <- RuntimeManager[F].computeBonds(preStateHash.toByteString)
        toSlash       = offenders intersect preStateBonds.filter { case (_, b) => b > 0 }.keySet
        _             <- Log[F].info(s"Slashing senders: [${toSlash.map(_.show).mkString("; ")}]")
        // epoch
        changeEpoch = epochLength % nextBlockNum == 0
        // attestation
        // no need to attest if nothing meaningful to finalize.
        dag <- BlockDagStorage[F].getRepresentation
        conflictSet = {
          val msgMap = dag.dagMessageState.msgMap
          parentHashes.flatMap(msgMap(_).seen) -- preState.fringe.flatMap(msgMap(_).seen)
        }
        nothingToFinalize = conflictSet.toList
          .traverse(BlockStore[F].getUnsafe)
          .map(_.forall { b =>
            b.state.systemDeploys.isEmpty && b.state.deploys.isEmpty
          })
        waitingForSupermajorityToAttest = {
          val newlySeen = creatorsLatestOpt
            .map { prev =>
              prev.justifications.flatMap(dag.dagMessageState.msgMap(_).seen) --
                parentHashes.flatMap(dag.dagMessageState.msgMap(_).seen)
            }
            .getOrElse(Set())
          newlySeen.toList.traverse(BlockStore[F].getUnsafe).map { newBlocks =>
            val newStateTransition =
              newBlocks.exists(b => b.state.systemDeploys.nonEmpty || b.state.deploys.nonEmpty)
            val attestationStake =
              preStateBonds.filterKeys(newBlocks.map(_.sender).toSet).values.toList.sum
            !newStateTransition && attestationStake * 3 < preStateBonds.values.toList.sum * 2
          }
        }
        suppressAttestation <- nothingToFinalize ||^ waitingForSupermajorityToAttest
//        // push dummy deploy if needed
//        p <- BlockDagStorage[F].pooledDeploys
//        _ <- dummyDeployOpt
//              .traverse {
//                case (privateKey, term) =>
//                  val deployData = ConstructDeploy.sourceDeployNow(
//                    source = term,
//                    sec = privateKey,
//                    vabn = nextBlockNum - 1,
//                    shardId = shardId
//                  )
//                  BlockDagStorage[F].addDeploy(deployData)
//              }
//              .whenA(p.isEmpty)
        // user deploys
        pooled <- BlockDagStorage[F].pooledDeploys
        pooledOk <- pooled.toList
                     .filterA {
                       case (id, d) =>
                         val future       = d.data.validAfterBlockNumber > nextBlockNum
                         val expired      = d.data.validAfterBlockNumber < nextBlockNum - MultiParentCasper.deployLifespan
                         val replayAttack = BlockDagStorage[F].lookupByDeployId(id).map(_.nonEmpty)
                         (future.pure ||^ expired.pure ||^ replayAttack).not
                     }
                     .map(_.map(_._1))
        deploys <- {
          val dummy = dummyDeployOpt
            .traverse {
              case (privateKey, term) =>
                val deployData = ConstructDeploy.sourceDeployNow(
                  source = term,
                  sec = privateKey,
                  vabn = nextBlockNum - 1,
                  shardId = shardId
                )
                BlockDagStorage[F].addDeploy(deployData).as(List(deployData.sig))
            }
          OptionT
            .fromOption(pooledOk.nonEmpty.guard[Option].as(pooledOk))
            .orElseF(dummy)
            .value
            .map(_.getOrElse(List()))
        }
        // create block
        _ <- Log[F].info(s"Creating block #${nextBlockNum} (seqNum ${nextSeqNum})")
        result <- BlockCreator(validatorIdentity, shardId).create(
                   preStateHash,
                   parentHashes,
                   finalBonds,
                   preState.fringeRejectedDeploys,
                   nextBlockNum,
                   nextSeqNum,
                   deploys,
                   toSlash,
                   changeEpoch,
                   suppressAttestation
                 )
      } yield result

    def validateBlock(block: BlockMessage) =
      MultiParentCasper.validate(block, shardId, minPhloPrice).flatMap { result =>
        result
          .map { blockMeta =>
            BlockDagStorage[F].insert(blockMeta, block).as(BlockStatus.valid.asRight[InvalidBlock])
          }
          .leftMap {
            case (_, err) =>
              FatalError(s"Failed to replay own block: $err").raiseError[F, ValidBlockProcessing]
          }
          .merge
      }

    def checkValidatorIsActive(validator: ValidatorIdentity): F[Boolean] =
      for {
        dag          <- BlockDagStorage[F].getRepresentation
        latestFringe = dag.dagMessageState.latestFringe
        // TODO: take bonds map from merged state of fringe
        //  - it should also include consensus bonds map
        bondsMap <- if (latestFringe.nonEmpty) latestFringe.head.bondsMap.pure[F]
                   else BlockDagStorage[F].lookupUnsafe(dag.heightMap.head._2.head).map(_.bondsMap)
        sender = ByteString.copyFrom(validator.publicKey.bytes)
      } yield bondsMap.contains(sender)

    val proposeEffect = (b: BlockMessage) =>
      // store block
      BlockStore[F].put(b) >>
        // save changes to Casper
        // TODO: temp this is done after validation
//        MultiParentCasper.handleValidBlock(b) >>
        // broadcast hash to peers
        CommUtil[F].sendBlockHash(b.blockHash, b.sender)

    new Proposer(
      getLatestSeqNumber,
      checkValidatorIsActive,
      createBlock,
      validateBlock,
      proposeEffect,
      validatorIdentity
    )
  }
}
