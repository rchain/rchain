package coop.rchain.node.revvaultexport

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.storage.RNodeKeyValueStoreManager.legacyRSpacePathPrefix
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

object StateBalances {

  def read[F[_]: Concurrent: Parallel: ContextShift](
      blockHash: String,
      vaultTreeHashMapDepth: Int,
      vaultChannel: Par,
      dataDir: Path
  )(implicit scheduler: ExecutionContext): F[List[(ByteString, Long)]] = {
    val oldRSpacePath = dataDir.resolve(s"$legacyRSpacePathPrefix/history/data.mdb")
    import coop.rchain.rholang.interpreter.storage._
    implicit val span                                        = NoopSpan[F]()
    implicit val log: Log[F]                                 = Log.log
    implicit val metrics                                     = new Metrics.MetricsNOP[F]()
    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    val legacyRSpaceDirSupport                               = Files.exists(oldRSpacePath)
    for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[F](dataDir, legacyRSpaceDirSupport)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      blockOpt          <- blockStore.get(ByteString.copyFrom(Base16.unsafeDecode(blockHash)))
      block             = blockOpt.get
      store             <- rnodeStoreManager.rSpaceStores
      spaces <- RSpace
                 .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                   store
                 )
      (rSpacePlay, rSpaceReplay) = spaces
      runtimes                   <- RhoRuntime.createRuntimes[F](rSpacePlay, rSpaceReplay, Seq.empty)
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
