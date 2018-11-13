package coop.rchain.casper.util.comm

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.ValidatorIdentity
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.{
  InternalProcessedDeploy,
  ProcessedDeployUtil,
  RuntimeManager
}
import coop.rchain.catscontrib.Capture
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.shared._

import scala.util.Try
import scala.language.higherKinds

/**
  * Validator side of the protocol defined in
  * https://rchain.atlassian.net/wiki/spaces/CORE/pages/485556483/Initializing+the+Blockchain+--+Protocol+for+generating+the+Genesis+block
  */
class BlockApproverProtocol[F[_]](
    validatorId: ValidatorIdentity,
    deployTimestamp: Long,
    runtimeManager: RuntimeManager[F],
    bonds: Map[Array[Byte], Long],
    wallets: Seq[PreWallet],
    minimumBond: Long,
    maximumBond: Long,
    faucet: Boolean,
    requiredSigs: Int
) {
  private implicit val logSource: LogSource = LogSource(this.getClass)
  private val _bonds                        = bonds.map(e => ByteString.copyFrom(e._1) -> e._2)

  def unapprovedBlockPacketHandler(
      peer: PeerNode,
      u: UnapprovedBlock
  )(
      implicit syncF: Sync[F],
      transportLayer: TransportLayer[F],
      logF: Log[F],
      rpConfAskF: RPConfAsk[F]
  ): F[Option[Packet]] =
    if (u.candidate.isEmpty) {
      Log[F]
        .warn("Candidate is not defined.")
        .map(_ => none[Packet])
    } else {
      val candidate = u.candidate.get
      for {
        validCandidate <- BlockApproverProtocol.validateCandidate(
                           runtimeManager,
                           candidate,
                           requiredSigs,
                           deployTimestamp,
                           wallets,
                           _bonds,
                           minimumBond,
                           maximumBond,
                           faucet
                         )

        result <- validCandidate match {
                   case Right(_) =>
                     for {
                       local <- RPConfAsk[F].reader(_.local)
                       serializedApproval = BlockApproverProtocol
                         .getApproval(candidate, validatorId)
                         .toByteString
                       msg = Blob(local, Packet(transport.BlockApproval.id, serializedApproval))
                       _   <- TransportLayer[F].stream(Seq(peer), msg)
                       _ <- Log[F].info(
                             s"Received expected candidate from $peer. Approval sent in response."
                           )
                     } yield none[Packet]
                   case Left(errMsg) =>
                     Log[F]
                       .warn(s"Received unexpected candidate from $peer because: $errMsg")
                       .map(_ => none[Packet])
                 }
      } yield result
    }
}

object BlockApproverProtocol {
  def getBlockApproval(
      expectedCandidate: ApprovedBlockCandidate,
      validatorId: ValidatorIdentity
  ): BlockApproval = {
    val sigData = Blake2b256.hash(expectedCandidate.toByteArray)
    val sig     = validatorId.signature(sigData)
    BlockApproval(Some(expectedCandidate), Some(sig))
  }

  def getApproval(
      candidate: ApprovedBlockCandidate,
      validatorId: ValidatorIdentity
  ): BlockApproval =
    getBlockApproval(candidate, validatorId)

  def validateCandidate[F[_]: Sync](
      runtimeManager: RuntimeManager[F],
      candidate: ApprovedBlockCandidate,
      requiredSigs: Int,
      timestamp: Long,
      wallets: Seq[PreWallet],
      bonds: Map[ByteString, Long],
      minimumBond: Long,
      maximumBond: Long,
      faucet: Boolean
  ): F[Either[String, Unit]] = {
    val blockDeploysValidation =
      for {
        _ <- (candidate.requiredSigs == requiredSigs)
              .either(())
              .or("Candidate didn't have required signatures number.")
        block      <- Either.fromOption(candidate.block, "Candidate block is empty.")
        body       <- Either.fromOption(block.body, "Body is empty")
        postState  <- Either.fromOption(body.postState, "Post state is empty")
        blockBonds = postState.bonds.map { case Bond(validator, stake) => validator -> stake }.toMap
        _ <- (blockBonds == bonds)
              .either(())
              .or("Block bonds don't match expected.")
        validators = blockBonds.toSeq.map(b => ProofOfStakeValidator(b._1.toByteArray, b._2))
        posParams  = ProofOfStakeParams(minimumBond, maximumBond, validators)
        faucetCode = if (faucet) Faucet.basicWalletFaucet _ else Faucet.noopFaucet
        genesisBlessedContracts = Genesis
          .defaultBlessedTerms(timestamp, posParams, wallets, faucetCode)
          .toSet
        blockDeploysResult    = body.deploys.flatMap(ProcessedDeployUtil.toInternal)
        genesisBlessedTerms   = genesisBlessedContracts.flatMap(_.term)
        genesisBlessedDeploys = genesisBlessedContracts.flatMap(_.raw)
        _ <- blockDeploysResult
              .forall(
                d =>
                  genesisBlessedTerms.contains(d.deploy.term.get) && genesisBlessedDeploys
                    .exists(dd => deployDataEq.eqv(dd, d.deploy.raw.get))
              )
              .either(())
              .or("Candidate deploys do not match expected deploys.")
        _ <- (blockDeploysResult.size == genesisBlessedContracts.size)
              .either(())
              .or("Mismatch between number of candidate deploys and expected number of deploys.")
      } yield (blockDeploysResult, postState)

    def validateStateHash(
        input: (Seq[InternalProcessedDeploy], RChainState)
    ): F[Either[String, Unit]] = {
      val (blockDeploys, postState) = input

      runtimeManager
        .replayComputeState(runtimeManager.emptyStateHash, blockDeploys)
        .map { stateHashResult =>
          for {
            stateHash <- stateHashResult.leftMap {
                          case (_, status) => s"Failed status during replay: $status."
                        }
            _ <- (stateHash == postState.tuplespace)
                  .either(())
                  .or("Tuplespace hash mismatch.")
          } yield ()
        }
    }

    def validateTuplespaceBonds(
        input: (Seq[InternalProcessedDeploy], RChainState)
    ): F[Either[String, Unit]] = {
      val (_, postState) = input

      runtimeManager
        .computeBonds(postState.tuplespace)
        .map { tuplespaceBonds =>
          val tuplespaceBondsMap = tuplespaceBonds.map {
            case Bond(validator, stake) => validator -> stake
          }.toMap

          (tuplespaceBondsMap == bonds)
            .either(())
            .or("Tuplespace bonds don't match expected ones.")
        }
    }

    for {
      stateHashValidation <- blockDeploysValidation.traverse(validateStateHash)

      tuplespaceBondsValidation <- blockDeploysValidation.traverse(validateTuplespaceBonds)
    } yield {
      for {
        _ <- stateHashValidation
        _ <- tuplespaceBondsValidation
      } yield ()
    }
  }

  def packetToUnapprovedBlock(msg: Packet): Option[UnapprovedBlock] =
    if (msg.typeId == transport.UnapprovedBlock.id)
      Try(UnapprovedBlock.parseFrom(msg.content.toByteArray)).toOption
    else None

  val deployDataEq: cats.kernel.Eq[DeployData] = (x: DeployData, y: DeployData) =>
    x.user.equals(y.user) &&
      x.timestamp === y.timestamp &&
      x.sig.equals(y.sig) &&
      x.sigAlgorithm === y.sigAlgorithm &&
      x.from === y.from &&
      x.phloPrice === y.phloPrice &&
      x.phloLimit === y.phloLimit &&
      x.nonce === y.nonce
}
