package coop.rchain.casper.api

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import org.mockito.ArgumentMatchersSugar.any
import org.mockito.IdiomaticMockito.StubbingOps
import org.mockito.MockitoSugar.mock

import scala.collection.immutable.SortedMap

object Mocks {

  def create[F[_]: Sync](): (BlockStore[F], BlockDagStorage[F], RuntimeManager[F]) =
    (createBlockStore[F], createBlockDagStorage[F], mock[RuntimeManager[F]])

  def createBlockStore[F[_]: Sync]: BlockStore[F] = {
    val state = Ref.unsafe[F, List[BlockMessage]](List())
    val bs    = mock[BlockStore[F]]

    bs.put(any) answers { kvPairs: Seq[(BlockHash, BlockMessage)] =>
      state.update(s => kvPairs.foldLeft(s) { case (acc, item) => acc :+ item._2 })
    }
    bs.get(any) answers { keys: Seq[BlockHash] =>
      state.get.map(s => keys.map(h => s.find(_.blockHash == h)))
    }
    bs
  }

  def createBlockDagStorage[F[_]: Sync]: BlockDagStorage[F] = {
    val genesisHash: ByteString =
      "9619d9a34bdaf56d5de8cfb7c2304d63cd9e469a0bfc5600fd2f5b9808e290f1".unsafeHexToByteString

    val state = Ref.unsafe[F, DagRepresentation](
      DagRepresentation(
        Set(),
        Map(),
        SortedMap(),
        DagMessageState(),
        Map(Set() -> (Blake2b256Hash.fromByteString(genesisHash), Set()))
      )
    )
    val bds = mock[BlockDagStorage[F]]

    bds.insert(any, invalid = false) answers { (b: BlockMessage) =>
      state.updateAndGet { s =>
        val newDagSet = s.dagSet + b.blockHash

        val newChildMap = b.justifications.foldLeft(s.childMap) {
          case (m, h) => m + (h -> (m.getOrElse(h, Set.empty) + b.blockHash))
        } + (b.blockHash -> Set.empty[BlockHash])

        val newHeightMap = s.heightMap + (b.blockNumber -> (s.heightMap
          .getOrElse(b.blockNumber, Set.empty) + b.blockHash))

        val seen = b.justifications
          .flatMap(h => s.dagMessageState.msgMap(h).seen)
          .toSet ++ b.justifications + b.blockHash

        val newMsgMap = s.dagMessageState.msgMap + (b.blockHash -> toMessage(b, seen))

        val newLatestMsgs = newMsgMap.foldLeft(Set.empty[Message[BlockHash, Validator]]) {
          case (acc, (_, msg)) =>
            acc + acc
              .find(_.sender == msg.sender)
              .map(m => if (msg.height > m.height) msg else m)
              .getOrElse(msg)
        }
        val newDagMessageState = s.dagMessageState.copy(newLatestMsgs, newMsgMap)

        s.copy(
          dagSet = newDagSet,
          childMap = newChildMap,
          heightMap = newHeightMap,
          dagMessageState = newDagMessageState
        )
      }
    }

    bds.getRepresentation returns state.get

    bds
  }

  // Default args only available for public method in Scala 2.12 (https://github.com/scala/bug/issues/12168)
  def toMessage(
      m: BlockMessage,
      seen: Set[BlockHash] = Set.empty[BlockHash]
  ): Message[BlockHash, Validator] =
    Message[BlockHash, Validator](
      m.blockHash,
      m.blockNumber,
      m.sender,
      m.seqNum,
      m.bonds,
      m.justifications.toSet,
      Set(),
      seen
    )
}
