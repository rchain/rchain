package coop.rchain.node.revvaultexport

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.BlockRandomSeed
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.rholang.RuntimeManager.emptyStateHashFixed
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.syntax._
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GString}
import coop.rchain.models.{ETuple, Expr}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._

import java.nio.file.Path
import scala.concurrent.ExecutionContext

object StateBalances {

  // the genesis vaultMap unforgeable name is no longer fixed for every chain.
  // It is undetermined and it depends on the genesis ceremony.
  // But we could try to get it from the extractState we added.
  def getGenesisVaultMapPar[F[_]: Sync](
      shardId: String,
      runtime: RhoRuntime[F]
  ): F[Par] = {
    // RevVault contract is the 7th contract deployed in the genesis, start from 0. Index should be 6
    val RevVaultContractDeployIndex: Byte = 6
    val revVault = {
      val rand = BlockRandomSeed
        .generateSplitRandomNumber(
          BlockRandomSeed(
            shardId,
            Genesis.genesisRandomSeedBlockNumber,
            Genesis.genesisPubKey,
            Blake2b256Hash.fromByteString(emptyStateHashFixed)
          ),
          RevVaultContractDeployIndex,
          BlockRandomSeed.UserDeploySplitIndex
        )
      val unfogeableBytes = rand.next()
      unfogeableBytes.toParUnforgeableName
    }

    val extractStateString = Par(exprs = Seq(Expr(GString("extractState"))))
    val e                  = Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(revVault, extractStateString))))))
    for {
      c <- runtime.getContinuation(Seq(e))
      unf = c.head.continuation.taggedCont.parBody.get.body.sends.head.data.head.exprs.head.getEMapBody.ps
        .get(Par(exprs = Seq(Expr(GString("vaultMap")))))
        .get
    } yield unf
  }

  def read[F[_]: Concurrent: Parallel: ContextShift](
      shardId: String,
      blockHash: String,
      vaultTreeHashMapDepth: Int,
      dataDir: Path
  )(implicit scheduler: ExecutionContext): F[List[(ByteString, Long)]] = {
    import coop.rchain.rholang.interpreter.storage._
    implicit val span                                        = NoopSpan[F]()
    implicit val log: Log[F]                                 = Log.log
    implicit val metrics                                     = new Metrics.MetricsNOP[F]()
    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[F](dataDir)
      blockStore        <- BlockStore(rnodeStoreManager)
      blockOpt          <- blockStore.get1(blockHash.unsafeHexToByteString)
      block             = blockOpt.get
      store             <- rnodeStoreManager.rSpaceStores
      spaces <- RSpace
                 .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                   store
                 )
      (rSpacePlay, rSpaceReplay) = spaces
      runtimes                   <- RhoRuntime.createRuntimes[F](rSpacePlay, rSpaceReplay, true, Seq.empty, Par())
      (rhoRuntime, _)            = runtimes
      vaultChannel               <- getGenesisVaultMapPar(shardId, rhoRuntime)
      _ <- rhoRuntime.reset(
            Blake2b256Hash.fromByteString(block.postStateHash)
          )
      balances <- VaultBalanceGetter.getAllVaultBalance(
                   vaultTreeHashMapDepth,
                   vaultChannel,
                   MainnetStoreTokenUnf,
                   rhoRuntime
                 )
    } yield balances
  }
}
