package coop.rchain.casper.util.comm

import cats.Traverse
import cats.data.EitherT
import cats.effect.Concurrent
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
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared._

import scala.util.Try

/**
  * Validator side of the protocol defined in
  * https://rchain.atlassian.net/wiki/spaces/CORE/pages/485556483/Initializing+the+Blockchain+--+Protocol+for+generating+the+Genesis+block
  */
class BlockApproverProtocol(
    validatorId: ValidatorIdentity,
    deployTimestamp: Long,
    bonds: Map[PublicKey, Long],
    wallets: Seq[PreWallet],
    minimumBond: Long,
    maximumBond: Long,
    faucet: Boolean,
    requiredSigs: Int
) {
  implicit private val logSource: LogSource = LogSource(this.getClass)
  private val _bonds                        = bonds.map(e => ByteString.copyFrom(e._1.bytes) -> e._2)

  def unapprovedBlockPacketHandler[F[_]: Concurrent: TransportLayer: Log: Time: ErrorHandler: RPConfAsk](
      peer: PeerNode,
      u: UnapprovedBlock,
      runtimeManager: RuntimeManager[F]
  ): F[Unit] =
    if (u.candidate.isEmpty) {
      Log[F].warn("Candidate is not defined.")
    } else {
      val candidate = u.candidate.get
      BlockApproverProtocol
        .validateCandidate(
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
        .flatMap {
          case Right(_) =>
            for {
              local <- RPConfAsk[F].reader(_.local)
              serializedApproval = BlockApproverProtocol
                .getApproval(candidate, validatorId)
                .toByteString
              msg = Blob(local, Packet(transport.BlockApproval.id, serializedApproval))
              _   <- TransportLayer[F].stream(peer, msg)
              _ <- Log[F].info(
                    s"Received expected candidate from $peer. Approval sent in response."
                  )
            } yield ()
          case Left(errMsg) =>
            Log[F].warn(s"Received unexpected candidate from $peer because: $errMsg")
        }
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

  def validateCandidate[F[_]: Concurrent](
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

    def validate: Either[String, (Seq[InternalProcessedDeploy], RChainState)] =
      for {
        _ <- (candidate.requiredSigs == requiredSigs)
              .either(())
              .or("Candidate didn't have required signatures number.")
        block      <- Either.fromOption(candidate.block, "Candidate block is empty.")
        body       <- Either.fromOption(block.body, "Body is empty")
        postState  <- Either.fromOption(body.state, "Post state is empty")
        blockBonds = postState.bonds.map { case Bond(validator, stake) => validator -> stake }.toMap
        _ <- (blockBonds == bonds)
              .either(())
              .or("Block bonds don't match expected.")
        validators = blockBonds.toSeq.map {
          case (pk, stake) =>
            Validator(PublicKey(pk.toByteArray), stake)
        }
        posParams      = ProofOfStake(minimumBond, maximumBond, validators)
        faucetCode     = if (faucet) Faucet.basicWalletFaucet(_) else Faucet.noopFaucet
        (_, genesisPk) = Ed25519.newKeyPair
        vaults = Traverse[List]
          .traverse(posParams.validators.map(_.pk).toList)(RevAddress.fromPublicKey)
          .get
          .map(Vault(_, 1000L))
        genesisBlessedContracts = Genesis
          .defaultBlessedTerms(
            timestamp,
            posParams,
            wallets,
            faucetCode,
            genesisPk,
            vaults,
            Long.MaxValue
          )
          .toSet
        blockDeploys          = body.deploys.flatMap(ProcessedDeployUtil.toInternal)
        genesisBlessedDeploys = genesisBlessedContracts
        _ <- (blockDeploys.size == genesisBlessedContracts.size)
              .either(())
              .or("Mismatch between number of candidate deploys and expected number of deploys.")
      } yield (blockDeploys, postState)

    (for {
      result                    <- EitherT(validate.pure[F])
      (blockDeploys, postState) = result
      stateHash <- EitherT(
                    runtimeManager
                      .replayComputeState(runtimeManager.emptyStateHash)(blockDeploys)
                  ).leftMap { case (_, status) => s"Failed status during replay: $status." }
      _ <- EitherT(
            (stateHash == postState.postStateHash)
              .either(())
              .or("Tuplespace hash mismatch.")
              .pure[F]
          )
      tuplespaceBonds <- EitherT(
                          Concurrent[F]
                            .attempt(runtimeManager.computeBonds(postState.postStateHash))
                        ).leftMap(_.getMessage)
      tuplespaceBondsMap = tuplespaceBonds.map {
        case Bond(validator, stake) => validator -> stake
      }.toMap
      _ <- EitherT(
            (tuplespaceBondsMap == bonds)
              .either(())
              .or("Tuplespace bonds don't match expected ones.")
              .pure[F]
          )
    } yield ()).value
  }

  def packetToUnapprovedBlock(msg: Packet): Option[UnapprovedBlock] =
    if (msg.typeId == transport.UnapprovedBlock.id)
      Try(UnapprovedBlock.parseFrom(msg.content.toByteArray)).toOption
    else None

  val deployDataEq: cats.kernel.Eq[DeployData] = new cats.kernel.Eq[DeployData] {
    override def eqv(x: DeployData, y: DeployData): Boolean =
      x.deployer.equals(y.deployer) &&
        x.timestamp === y.timestamp &&
        x.sig.equals(y.sig) &&
        x.sigAlgorithm === y.sigAlgorithm &&
        x.phloPrice === y.phloPrice &&
        x.phloLimit === y.phloLimit
  }
}
