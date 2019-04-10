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
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.util.RevAddress
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
    buildGenesis(
      Genesis(
        shardId = "HashSetCasperTest",
        timestamp = 0L,
        wallets = Seq.empty[PreWallet],
        proofOfStake = ProofOfStake(
          minimumBond = 0L,
          maximumBond = Long.MaxValue,
          validators = bonds.toSeq.map(Validator.tupled)
        ),
        faucet = false,
        genesisPk = Ed25519.newKeyPair._2,
        vaults = bonds.toList.map {
          case (pk, stake) =>
            RevAddress.fromPublicKey(pk).map(Vault(_, stake))
        }.flattenOption,
        supply = Long.MaxValue
      )
    )

  def buildGenesisParameters(
      users: Int,
      validatorPks: Seq[PublicKey],
      bonds: Map[PublicKey, Long]
  ): Genesis = {
    val (_, ethPubKeys) = (1 to users).map(_ => Secp256k1.newKeyPair).unzip
    val ethAddresses =
      ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
    Genesis(
      shardId = "MultiParentCasperSpec",
      timestamp = 0L,
      wallets = ethAddresses.map(PreWallet(_, BigInt(10001))),
      proofOfStake = ProofOfStake(
        minimumBond = 0L,
        maximumBond = Long.MaxValue,
        validators = bonds.map(Validator.tupled).toSeq
      ),
      faucet = true,
      genesisPk = Ed25519.newKeyPair._2,
      vaults = bonds.toList.map {
        case (pk, stake) =>
          RevAddress.fromPublicKey(pk).map(Vault(_, stake))
      }.flattenOption,
      supply = Long.MaxValue
    )
  }

  def buildGenesis(genesisParameters: Genesis): BlockMessage = {
    val storageDirectory                   = Files.createTempDirectory(s"hash-set-casper-test-genesis")
    val storageSize: Long                  = 3024L * 1024
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
      genesis <- Genesis.createGenesisBlock(
                  runtimeManager,
                  genesisParameters
                )
      _ <- activeRuntime.close()
    } yield genesis).unsafeRunSync
  }
}
