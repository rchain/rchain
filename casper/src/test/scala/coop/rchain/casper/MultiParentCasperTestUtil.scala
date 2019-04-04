package coop.rchain.casper

import java.nio.file.Files

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.crypto.PublicKey
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.shared.{Log, StoreType}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

object MultiParentCasperTestUtil {
  def validateBlockStore[R](
      node: HashSetCasperTestNode[Effect]
  )(f: BlockStore[Effect] => Effect[R])(implicit metrics: Metrics[Effect], log: Log[Effect]) =
    for {
      bs     <- BlockDagStorageTestFixture.createBlockStorage[Effect](node.blockStoreDir)
      result <- f(bs)
      _      <- bs.close()
      _      <- Sync[Effect].delay { node.blockStoreDir.recursivelyDelete() }
    } yield result

  def blockTuplespaceContents(
      block: BlockMessage
  )(implicit casper: MultiParentCasper[Effect]): Effect[String] = {
    val tsHash = ProtoUtil.postStateHash(block)
    MultiParentCasper[Effect].storageContents(tsHash)
  }

  def deployAndQuery(
      node: HashSetCasperTestNode[Effect],
      dd: DeployData,
      query: DeployData
  ): Effect[(BlockStatus, Seq[Par])] =
    for {
      createBlockResult <- node.casperEff.deploy(dd) >> node.casperEff.createBlock
      Created(block)    = createBlockResult
      blockStatus       <- node.casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
      queryResult <- node.runtimeManager
                      .captureResults(ProtoUtil.postStateHash(block), query)
    } yield (blockStatus, queryResult)

  def createBonds(validators: Seq[PublicKey]): Map[PublicKey, Long] =
    validators.zipWithIndex.map { case (v, i) => v -> (2L * i.toLong + 1L) }.toMap

  def createGenesis(bonds: Map[PublicKey, Long]): BlockMessage =
    buildGenesis(Seq.empty, bonds, 1L, Long.MaxValue, false, 0L)

  def buildGenesis(
      wallets: Seq[PreWallet],
      bonds: Map[PublicKey, Long],
      minimumBond: Long,
      maximumBond: Long,
      faucet: Boolean,
      deployTimestamp: Long
  ): BlockMessage = {
    val storageDirectory                   = Files.createTempDirectory(s"hash-set-casper-test-genesis")
    val storageSize: Long                  = 1024L * 1024
    implicit val log: Log.NOPLog[Task]     = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    (for {
      activeRuntime <- Runtime
                        .createWithEmptyCost[Task, Task.Par](
                          storageDirectory,
                          storageSize,
                          StoreType.LMDB
                        )
      runtimeManager <- RuntimeManager.fromRuntime[Task](activeRuntime)
      validators     = bonds.toSeq.map(Validator.tupled)
      genesis <- Genesis.createGenesisBlock(
                  runtimeManager,
                  Genesis(
                    "HashSetCasperTest-shard",
                    deployTimestamp,
                    wallets,
                    ProofOfStake(minimumBond, maximumBond, validators),
                    faucet
                  )
                )
      _ <- activeRuntime.close()
    } yield genesis).unsafeRunSync
  }
}
