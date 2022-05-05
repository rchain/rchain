package coop.rchain.node.revvaultexport

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.syntax._
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._

import java.nio.file.Path
import scala.concurrent.ExecutionContext

object StateBalances {

  def read[F[_]: Concurrent: Parallel: ContextShift](
      blockHash: String,
      vaultTreeHashMapDepth: Int,
      vaultChannel: Par,
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
      _ <- rhoRuntime.reset(
            Blake2b256Hash.fromByteString(block.body.state.postStateHash)
          )
      balances <- VaultBalanceGetter.getAllVaultBalance(
                   vaultTreeHashMapDepth,
                   vaultChannel,
                   rhoRuntime
                 )
    } yield balances
  }
}
