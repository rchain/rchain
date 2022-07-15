package coop.rchain.casper.engine

import cats.Parallel
import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.blocks.BlockRetriever
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Registry, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockMetadata
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import fs2.Stream
import fs2.concurrent.Queue

import scala.concurrent.duration.DurationInt

final case class PeerMessage(peer: PeerNode, message: CasperMessage)

object NodeLaunch {

  // format: off
  def apply[F[_]
    /* Execution */   : Concurrent: Parallel: ContextShift: Time: Timer
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever
    /* State */       : RPConfAsk: ConnectionsCell
    /* Rholang */     : RuntimeManager
    /* Storage */     : BlockStore: ApprovedStore: BlockDagStorage: RSpaceStateManager
    /* Diagnostics */ : Log: Metrics: Span] // format: on
  (
      packets: Stream[F, PeerMessage],
      incomingBlocksQueue: Queue[F, BlockMessage],
      conf: CasperConf,
      trimState: Boolean,
      disableStateExporter: Boolean,
      validatorIdentityOpt: Option[ValidatorIdentity],
      standalone: Boolean
  ): F[Unit] = {

    def noValidatorIdentityError =
      new Exception("To create genesis block node must provide validator private key")

    def createStoreBroadcastGenesis: F[Unit] =
      for {
        _ <- Log[F]
              .warn(
                "Public key for PoS vault is not set, manual transfer from PoS vault will be disabled (config 'pos-vault-pub-key')"
              )
              .whenA(conf.genesisBlockData.posVaultPubKey.isEmpty)
        _ <- Log[F]
              .warn(
                "Public key for system - contract is not set (config 'system-contract-pub-key')"
              )
              .whenA(conf.genesisBlockData.systemContractPubKey.isEmpty)

        // Get creator public key
        validatorIdentity <- validatorIdentityOpt.liftTo(noValidatorIdentityError)
        genesisBlock      <- createGenesisBlockFromConfig(validatorIdentity, conf)
        genBlockStr       = PrettyPrinter.buildString(genesisBlock)
        _                 <- Log[F].info(s"Sending genesis $genBlockStr to peers...")

        bmd = BlockMetadata
          .fromBlock(genesisBlock)
          // Genesis pre-state is used as finalized state hash
          .copy(fringeStateHash = genesisBlock.preStateHash)

        // Store genesis block
        _             <- BlockStore[F].put(genesisBlock)
        genesisFringe = FinalizedFringe(bmd.fringe, bmd.fringeStateHash)
        _             <- ApprovedStore[F].putApprovedBlock(genesisFringe)
        // Add genesis block to DAG
        _ <- BlockDagStorage[F].insertNew(bmd, genesisBlock)

        // Send approved block to peers
        _ <- CommUtil[F].streamToPeers(genesisFringe.toProto)
      } yield ()

    def startSyncingMode: F[Unit] =
      for {
        validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
        finished    <- Deferred[F, Unit]
        engine      <- NodeSyncing[F](finished, validatorId, trimState)
        handleMessages = packets.parEvalMapUnorderedProcBounded { pm =>
          engine.handle(pm.peer, pm.message)
        }
        _ <- CommUtil[F].requestFinalizedFringe(trimState)
        _ <- (Stream.eval(finished.get) concurrently handleMessages).compile.drain
      } yield ()

    def startRunningMode: F[Unit] =
      for {
        engine <- NodeRunning[F](incomingBlocksQueue, validatorIdentityOpt, disableStateExporter)
        handleMessages = packets.parEvalMapUnorderedProcBounded { pm =>
          engine.handle(pm.peer, pm.message)
        }
        _ <- Log[F].info(s"Making a transition to Running state.")

        // Wait for first connection before sending requests or handling incoming messages
        // TODO: this is part of legacy logic, send FCT request to at least one node
        //  - this should be part of network layer to ensure messages are sent to right peers
        _ <- waitForFirstConnection
        _ <- CommUtil[F].sendForkChoiceTipRequest

        _ <- handleMessages.compile.drain
      } yield ()

    def waitForFirstConnection: F[Unit] =
      for {
        isEmpty <- ConnectionsCell[F].get.map(_.isEmpty)
        _       <- (Timer[F].sleep(250.millis) *> waitForFirstConnection).whenA(isEmpty)
      } yield ()

    for {
      dag <- BlockDagStorage[F].getRepresentation
      _ <- if (dag.dagSet.isEmpty && standalone) {
            // Create genesis block and send to peers
            Log[F].info("Starting as genesis master, creating genesis block...") *>
              createStoreBroadcastGenesis
          } else if (dag.dagSet.isEmpty) {
            // If state is empty, transition to syncing mode (LFS)
            Log[F].info("Starting from bootstrap node, syncing LFS...") *>
              startSyncingMode
          } else {
            Log[F].info("Reconnecting to existing network...")
          }

      // Transition to running mode
      _ <- startRunningMode
    } yield ()
  }

  def createGenesisBlockFromConfig[F[_]: Concurrent: ContextShift: RuntimeManager: Log](
      validator: ValidatorIdentity,
      conf: CasperConf
  ): F[BlockMessage] =
    createGenesisBlock[F](
      validator,
      conf.shardName,
      conf.genesisBlockData.genesisBlockNumber,
      conf.genesisBlockData.bondsFile,
      conf.autogenShardSize,
      conf.genesisBlockData.walletsFile,
      conf.genesisBlockData.bondMinimum,
      conf.genesisBlockData.bondMaximum,
      conf.genesisBlockData.epochLength,
      conf.genesisBlockData.quarantineLength,
      conf.genesisBlockData.numberOfActiveValidators,
      conf.genesisBlockData.posMultiSigPublicKeys,
      conf.genesisBlockData.posMultiSigQuorum,
      conf.genesisBlockData.posVaultPubKey,
      conf.genesisBlockData.systemContractPubKey
    )

  def createGenesisBlock[F[_]: Concurrent: ContextShift: RuntimeManager: Log](
      validator: ValidatorIdentity,
      shardId: String,
      blockNumber: Long,
      bondsPath: String,
      autogenShardSize: Int,
      vaultsPath: String,
      minimumBond: Long,
      maximumBond: Long,
      epochLength: Int,
      quarantineLength: Int,
      numberOfActiveValidators: Int,
      posMultiSigPublicKeys: List[String],
      posMultiSigQuorum: Int,
      posVaultPubKey: String,
      systemContractPubkey: String
  ): F[BlockMessage] =
    for {
      // Initial REV vaults
      vaults <- VaultParser.parse[F](vaultsPath)

      // Initial validators
      bonds      <- BondsParser.parse[F](bondsPath, autogenShardSize)
      validators = bonds.toSeq.map(Validator.tupled)

      // Run genesis deploys and create block
      genesisBlock <- Genesis.createGenesisBlock(
                       validator,
                       Genesis(
                         sender = validator.publicKey,
                         shardId = shardId,
                         proofOfStake = ProofOfStake(
                           minimumBond = minimumBond,
                           maximumBond = maximumBond,
                           epochLength = epochLength,
                           quarantineLength = quarantineLength,
                           numberOfActiveValidators = numberOfActiveValidators,
                           validators = validators,
                           posMultiSigPublicKeys = posMultiSigPublicKeys,
                           posMultiSigQuorum = posMultiSigQuorum,
                           posVaultPubKey = posVaultPubKey
                         ),
                         registry = Registry(systemContractPubkey),
                         vaults = vaults,
                         blockNumber = blockNumber
                       )
                     )
    } yield genesisBlock
}
