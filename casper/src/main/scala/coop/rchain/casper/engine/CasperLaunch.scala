package coop.rchain.casper.engine

import cats.Parallel
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.approvedStore.ApprovedStore
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.engine.Engine.transitionToRunning
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.state.RSpaceStateManager
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import fs2.Stream
import fs2.concurrent.Queue

object CasperLaunch {

  // format: off
  def of[F[_]
    /* Execution */   : Concurrent: Parallel: ContextShift: Time: Timer
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever: EventPublisher
    /* State */       : RPConfAsk: ConnectionsCell: LastApprovedBlock
    /* Rholang */     : RuntimeManager
    /* Casper */      : LastFinalizedHeightConstraintChecker: SynchronyConstraintChecker
    /* Storage */     : BlockStore: ApprovedStore: BlockDagStorage: DeployStorage: CasperBufferStorage: RSpaceStateManager
    /* Diagnostics */ : Log: EventLog: Metrics: Span] // format: on
  (
      packets: Stream[F, PeerMessage],
      blockProcessingQueue: Queue[F, BlockMessage],
      blocksInProcessing: Ref[F, Set[BlockHash]],
      conf: CasperConf,
      trimState: Boolean,
      disableStateExporter: Boolean,
      validatorIdentityOpt: Option[ValidatorIdentity],
      casperShardConf: CasperShardConf,
      standalone: Boolean
  ): F[Unit] = {

    def createGenesisAndBroadcast =
      for {
        genesisBlock   <- createGenesisBlockFromConfig(conf)
        genBlockStr    = PrettyPrinter.buildString(genesisBlock)
        _              <- Log[F].info(s"Sending genesis $genBlockStr to peers...")
        genBlockPacket = ToPacket(genesisBlock.toProto)

        // Store genesis
        _  <- BlockStore[F].put(genesisBlock)
        ab = ApprovedBlock(ApprovedBlockCandidate(genesisBlock, 0), Nil)
        _  <- ApprovedStore[F].putApprovedBlock(ab)
        _  <- LastApprovedBlock[F].set(ab)
        _  <- BlockDagStorage[F].insert(genesisBlock, invalid = false, approved = true)

        _ <- CommUtil[F].streamToPeers(genBlockPacket)
      } yield ()

    def initializeFromEmptyState: F[Unit] =
      for {
        initFinished <- Deferred[F, Unit]
        initEngine   <- startInitializing(initFinished, trimState)
        initRunning = packets.parEvalMapUnorderedProcBounded { pm =>
          initEngine.handle(pm.peer, pm.message)
        }
        initStream = Stream.eval(initFinished.get) concurrently initRunning
        _          <- initStream.compile.drain
      } yield ()

    def startInitializing(finished: Deferred[F, Unit], trimState: Boolean): F[Initializing[F]] =
      for {
        validatorId <- ValidatorIdentity.fromPrivateKeyWithLogging[F](conf.validatorPrivateKey)
        engine <- Engine.transitionToInitializing(
                   finished,
                   blockProcessingQueue,
                   blocksInProcessing,
                   casperShardConf,
                   validatorId,
                   trimState
                 )
        _ <- CommUtil[F].requestApprovedBlock(trimState)
      } yield engine

    def createRunning: F[Unit] =
      for {
        engine <- transitionToRunning[F](
                   blockProcessingQueue,
                   blocksInProcessing,
                   validatorIdentityOpt,
                   disableStateExporter
                 )
        _ <- CommUtil[F].sendForkChoiceTipRequest
        engineRun = packets.parEvalMapUnorderedProcBounded { pm =>
          engine.handle(pm.peer, pm.message)
        }
        _ <- Log[F].info(s"Making a transition to Running state.")
        _ <- engineRun.compile.drain
      } yield ()

    def connectToExistingNetwork: F[Unit] =
      for {
        // Ask peers for fork choice tips
        _ <- CommUtil[F].sendForkChoiceTipRequest

        pendants <- CasperBufferStorage[F].getPendants
        // pendantsReceived are either
        // 1. blocks that were received while catching up but not end up in casper buffer, e.g. node were restarted
        // or
        // 2. blocks which dependencies are in DAG, so they can be added to DAG
        // In both scenarios the way to proceed is to send them to Casper
        pendantsStored <- pendants.toList.filterA(BlockStore[F].contains(_))
        _ <- Log[F].info(
              s"Checking pendant hashes: ${pendantsStored.size} items in CasperBuffer."
            )
        _ <- pendantsStored
            // we just need to send blocks to Casper. Nothing to do with results of block processing here,
            // so ignoring them
              .traverse_(
                hash =>
                  for {
                    block <- BlockStore[F].get1(hash).map(_.get)
                    _ <- Log[F].info(
                          s"Pendant ${PrettyPrinter.buildString(block, short = true)} " +
                            s"is available in BlockStore, sending to Casper."
                        )
                    dag <- BlockDagStorage[F].getRepresentation
                    dc  = dag.contains(hash)
                    _ <- Log[F]
                          .error(
                            s"Pendant ${PrettyPrinter.buildString(block, short = true)} " +
                              s"is available in DAG, database is supposedly in inconsistent state."
                          )
                          .whenA(dc)
                    _ <- BlockRetriever[F].ackReceive(hash)
                    _ <- blockProcessingQueue.enqueue1(block)
                  } yield ()
              )
      } yield ()

    for {
      dag <- BlockDagStorage[F].getRepresentation
      _ <- if (dag.dagSet.isEmpty && standalone) {
            // Create genesis block and send to peers
            Log[F].info("Starting as genesis master, creating genesis block...") *>
              createGenesisAndBroadcast
          } else if (dag.dagSet.isEmpty) {
            // If DAG is empty start from initializing (LFS sync)
            Log[F].info("Starting from bootstrap node, syncing LFS...") *>
              initializeFromEmptyState
          } else {
            Log[F].info("Reconnecting to existing network")
          }
      _ <- connectToExistingNetwork
      _ <- Log[F].info("Starting running mode")
      _ <- createRunning
    } yield ()
  }

  def createGenesisBlockFromConfig[F[_]: Concurrent: ContextShift: Time: RuntimeManager: Log](
      conf: CasperConf
  ): F[BlockMessage] =
    createGenesisBlock[F](
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
      conf.genesisBlockData.posMultiSigQuorum
    )

  def createGenesisBlock[F[_]: Concurrent: ContextShift: Time: RuntimeManager: Log](
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
      posMultiSigQuorum: Int
  ): F[BlockMessage] =
    for {
      blockTimestamp <- Time[F].currentMillis

      // Initial REV vaults
      vaults <- VaultParser.parse[F](vaultsPath)

      // Initial validators
      bonds      <- BondsParser.parse[F](bondsPath, autogenShardSize)
      validators = bonds.toSeq.map(Validator.tupled)

      // Run genesis deploys and create block
      genesisBlock <- Genesis.createGenesisBlock(
                       Genesis(
                         shardId = shardId,
                         blockTimestamp = blockTimestamp,
                         proofOfStake = ProofOfStake(
                           minimumBond = minimumBond,
                           maximumBond = maximumBond,
                           epochLength = epochLength,
                           quarantineLength = quarantineLength,
                           numberOfActiveValidators = numberOfActiveValidators,
                           validators = validators,
                           posMultiSigPublicKeys = posMultiSigPublicKeys,
                           posMultiSigQuorum = posMultiSigQuorum
                         ),
                         vaults = vaults,
                         blockNumber = blockNumber
                       )
                     )
    } yield genesisBlock
}
