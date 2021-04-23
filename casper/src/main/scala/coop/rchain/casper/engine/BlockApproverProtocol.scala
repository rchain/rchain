package coop.rchain.casper.engine

import cats.MonadError
import cats.data.EitherT
import cats.effect.Concurrent
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.ValidatorIdentity
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.shared._

/**
  * Validator side of the protocol defined in
  * https://rchain.atlassian.net/wiki/spaces/CORE/pages/485556483/Initializing+the+Blockchain+--+Protocol+for+generating+the+Genesis+block
  */
final case class BlockApproverProtocol private (
    validatorId: ValidatorIdentity,
    deployTimestamp: Long,
    vaults: Seq[Vault],
    bonds: Map[PublicKey, Long],
    minimumBond: Long,
    maximumBond: Long,
    epochLength: Int,
    quarantineLength: Int,
    numberOfActiveValidators: Int,
    requiredSigs: Int
) {
  implicit private val logSource: LogSource = LogSource(this.getClass)
  private val _bonds                        = bonds.map(e => ByteString.copyFrom(e._1.bytes) -> e._2)

  def unapprovedBlockPacketHandler[F[_]: Concurrent: TransportLayer: Log: Time: RPConfAsk: RuntimeManager](
      peer: PeerNode,
      u: UnapprovedBlock
  ): F[Unit] = {
    val candidate = u.candidate
    Log[F].info(s"Received expected genesis block candidate from $peer. Verifying...") >>
      BlockApproverProtocol
        .validateCandidate(
          candidate,
          requiredSigs,
          deployTimestamp,
          vaults,
          _bonds,
          minimumBond,
          maximumBond,
          epochLength,
          quarantineLength,
          numberOfActiveValidators
        )
        .flatMap {
          case Right(_) =>
            for {
              local <- RPConfAsk[F].reader(_.local)
              serializedApproval = BlockApproverProtocol
                .getApproval(candidate, validatorId)
                .toProto
              msg = Blob(local, ToPacket(serializedApproval))
              _   <- TransportLayer[F].stream(peer, msg)
              _ <- Log[F].info(
                    s"Approved genesis block candidate from $peer. " +
                      s"Approval sent in response."
                  )
            } yield ()
          case Left(errMsg) =>
            Log[F].warn(s"Received unexpected genesis block candidate from $peer because: $errMsg")
        }
  }
}

object BlockApproverProtocol {
  def of[F[_]](
      validatorId: ValidatorIdentity,
      deployTimestamp: Long,
      vaults: Seq[Vault],
      bonds: Map[PublicKey, Long],
      minimumBond: Long,
      maximumBond: Long,
      epochLength: Int,
      quarantineLength: Int,
      numberOfActiveValidators: Int,
      requiredSigs: Int
  )(implicit monadError: MonadError[F, Throwable]): F[BlockApproverProtocol] =
    if (bonds.size > requiredSigs)
      new BlockApproverProtocol(
        validatorId,
        deployTimestamp,
        vaults,
        bonds,
        minimumBond,
        maximumBond,
        epochLength,
        quarantineLength,
        numberOfActiveValidators,
        requiredSigs
      ).pure[F]
    else
      monadError.raiseError(
        new Exception("Required sigs must be smaller than the number of bonded validators")
      )

  def getBlockApproval(
      expectedCandidate: ApprovedBlockCandidate,
      validatorId: ValidatorIdentity
  ): BlockApproval = {
    val sigData = Blake2b256.hash(expectedCandidate.toProto.toByteArray)
    val sig     = validatorId.signature(sigData)
    BlockApproval(expectedCandidate, sig)
  }

  def getApproval(
      candidate: ApprovedBlockCandidate,
      validatorId: ValidatorIdentity
  ): BlockApproval =
    getBlockApproval(candidate, validatorId)

  def validateCandidate[F[_]: Concurrent](
      candidate: ApprovedBlockCandidate,
      requiredSigs: Int,
      timestamp: Long,
      vaults: Seq[Vault],
      bonds: Map[ByteString, Long],
      minimumBond: Long,
      maximumBond: Long,
      epochLength: Int,
      quarantineLength: Int,
      numberOfActiveValidators: Int
  )(implicit runtimeManager: RuntimeManager[F]): F[Either[String, Unit]] = {

    def validate: Either[String, (Seq[ProcessedDeploy], RChainState)] =
      for {
        _ <- (candidate.requiredSigs == requiredSigs)
              .either(())
              .or("Candidate didn't have required signatures number.")
        block = candidate.block
        _ <- (block.body.systemDeploys.isEmpty)
              .either(())
              .or("Candidate must not contain system deploys.")
        blockBonds = block.body.state.bonds.map {
          case Bond(validator, stake) => validator -> stake
        }.toMap
        _ <- (blockBonds == bonds)
              .either(())
              .or("Block bonds don't match expected.")
        validators = blockBonds.toSeq.map {
          case (pk, stake) =>
            Validator(PublicKey(pk.toByteArray), stake)
        }
        posParams = ProofOfStake(
          minimumBond,
          maximumBond,
          validators,
          epochLength,
          quarantineLength,
          numberOfActiveValidators
        )
        genesisBlessedContracts = Genesis
          .defaultBlessedTerms(
            timestamp,
            posParams,
            vaults,
            Long.MaxValue
          )
        blockDeploys = block.body.deploys
        _ <- (blockDeploys.size == genesisBlessedContracts.size)
              .either(())
              .or("Mismatch between number of candidate deploys and expected number of deploys.")

        // check if expected blessed contracts and ones reported by candidate are equal
        // order should be the same and terms should be the same
        wrongDeploys = (blockDeploys.iterator zip genesisBlessedContracts.iterator)
          .filter {
            case (candidateDeploy, expectedContract) =>
              candidateDeploy.deploy.data.term != expectedContract.data.term
          }
          .map {
            case (candidateDeploy, _) =>
              candidateDeploy.deploy.data.term.substring(0, 100)
          }
          .take(5)

        _ <- wrongDeploys.isEmpty
              .either(())
              .or(
                s"Genesis candidate deploys do not match expected blessed contracts.\nBad contracts (5 first):\n${wrongDeploys
                  .mkString("\n")}"
              )
      } yield (blockDeploys, block.body.state)

    (for {
      result                    <- EitherT(validate.pure[F])
      (blockDeploys, postState) = result
      emptyStateHash            <- EitherT.right(RuntimeManager.emptyStateHashFixed.pure[F])
      // check if candidate blessed contracts match candidate postStateHash
      stateHash <- EitherT(
                    runtimeManager
                      .replayComputeState(emptyStateHash)(
                        blockDeploys,
                        List.empty,
                        BlockData.fromBlock(candidate.block),
                        Map.empty[BlockHash, Validator],
                        isGenesis = true
                      )
                  ).leftMap { status =>
                    s"Failed status during replay: $status."
                  }
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

  val deployDataEq: cats.kernel.Eq[DeployData] = new cats.kernel.Eq[DeployData] {
    override def eqv(x: DeployData, y: DeployData): Boolean =
      x.timestamp === y.timestamp &&
        x.phloPrice === y.phloPrice &&
        x.phloLimit === y.phloLimit
  }
}
