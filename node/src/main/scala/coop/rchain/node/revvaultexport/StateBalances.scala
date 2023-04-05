package coop.rchain.node.revvaultexport

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.rholang.BlockRandomSeed
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GString}
import coop.rchain.models.syntax._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._

import java.nio.file.Path
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global

object StateBalances {

  // the genesis vaultMap unforgeable name is no longer fixed for every chain.
  // It is undetermined and it depends on the genesis ceremony.
  // But we could try to get it from the extractState we added.
  def getGenesisVaultMapPar[F[_]: Sync](
      shardId: String,
      runtime: RhoRuntime[F]
  ): F[Par] = {
    val revVaultUnf        = BlockRandomSeed.revVaultUnforgeable(shardId)
    val extractStateString = Par(exprs = Seq(Expr(GString("extractState"))))
    val e                  = Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(revVaultUnf, extractStateString))))))
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
  ): F[List[(ByteString, Long)]] = {
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
                   store,
                   global
                 )
      (rSpacePlay, rSpaceReplay) = spaces
      runtimes                   <- RhoRuntime.createRuntimes[F](rSpacePlay, rSpaceReplay, true, Seq.empty, Par())
      (rhoRuntime, _)            = runtimes
      vaultChannel               <- getGenesisVaultMapPar(shardId, rhoRuntime)
      _                          <- rhoRuntime.reset(block.postStateHash.toBlake2b256Hash)
      balances <- VaultBalanceGetter.getAllVaultBalance(
                   vaultTreeHashMapDepth,
                   vaultChannel,
                   BlockRandomSeed.storeTokenUnforgeable(shardId),
                   rhoRuntime
                 )
    } yield balances
  }
}
