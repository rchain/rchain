package coop.rchain.casper.rholang.syntax

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic.{
  calculateNumChannelDiff,
  codecMergeableKey,
  DeployMergeableData,
  NumberChannel
}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.EventLogMergingLogic.{NumberChannelsDiff, NumberChannelsEndVal}
import coop.rchain.shared.AttemptOpsF.RichAttempt
import coop.rchain.shared.syntax._
import scodec.bits.ByteVector

trait RuntimeManagerSyntax {
  implicit final def casperSyntaxRuntimeManager[F[_]](rm: RuntimeManager[F]): RuntimeManagerOps[F] =
    new RuntimeManagerOps[F](rm)
}

// RuntimeManager extensions / syntax
final class RuntimeManagerOps[F[_]](private val rm: RuntimeManager[F]) extends AnyVal {

  /**
    * Load mergeable channels from store
    */
  def loadMergeableChannels(stateHashBS: StateHash, creator: Array[Byte], seqNum: Int)(
      implicit s: Sync[F]
  ): F[Seq[NumberChannelsDiff]] = {
    val stateHash = stateHashBS.toBlake2b256Hash

    val getKey =
      codecMergeableKey
        .encode((stateHash.bytes, ByteVector(creator), seqNum))
        .get
        .map(_.toByteVector)

    def mergeableStoreError =
      new Exception(s"Mergeable store invalid state hash ${stateHash.bytes.toHex}.")

    for {
      key    <- getKey
      resOpt <- rm.getMergeableStore.get1(key)
      res    <- resOpt.liftTo(mergeableStoreError)
      resMrg = res.map(_.channels.map(x => (x.hash, x.diff)).toMap)
    } yield resMrg
  }

  /**
    * Converts final mergeable (number) channel values and save to mergeable store.
    *
    * Tuple (postStateHash, creator, seqNum) is used as a key, preStateHash is used to
    * read initial value to get the difference.
    */
  def saveMergeableChannels(
      postStateHash: Blake2b256Hash,
      creator: Array[Byte],
      seqNum: Int,
      channelsData: Seq[NumberChannelsEndVal],
      // Used to calculate value difference from final values
      preStateHash: Blake2b256Hash
  )(implicit s: Concurrent[F]): F[Unit] =
    for {
      // Calculate difference values from final values on number channels
      diffs <- convertNumberChannelsToDiff(channelsData, preStateHash)

      // Convert to storage types
      deployChannels = diffs.map { data =>
        val channels = data.map(NumberChannel.tupled)
        DeployMergeableData(channels.toList)
      }

      // Key is composed from post-state hash and block creator with seq number
      key        = (postStateHash.bytes, ByteVector(creator), seqNum)
      keyEncoded <- codecMergeableKey.encode(key).get.map(_.toByteVector)

      // Save to mergeable channels store
      _ <- rm.getMergeableStore.put(keyEncoded, deployChannels)
    } yield ()

  /**
    * Converts number channels final values to difference values. Excludes channels without an initial value.
    *
    * @param channelsData Final values
    * @param preStateHash Inital state
    * @return Map with values as difference on number channel
    */
  def convertNumberChannelsToDiff(
      channelsData: Seq[NumberChannelsEndVal],
      // Used to calculate value difference from final values
      preStateHash: Blake2b256Hash
  )(implicit s: Concurrent[F]): F[List[NumberChannelsDiff]] = Sync[F].defer {
    // Get number channel value for pre-state
    val getDataFunc =
      (ch: Blake2b256Hash) =>
        rm.getHistoryRepo.getHistoryReader(preStateHash).flatMap(_.getData(ch))
    val getNumFunc = RholangMergingLogic.convertToReadNumber(getDataFunc)

    // Calculate difference values from final values on number channels
    calculateNumChannelDiff(channelsData, getNumFunc)
  }
}
