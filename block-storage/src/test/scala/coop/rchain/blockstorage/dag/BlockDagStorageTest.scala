package coop.rchain.blockstorage.dag

import java.nio.file.StandardOpenOption

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.LatestMessagesLogIsCorrupted
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.syntax._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits._
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.PathOps._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scodec.codecs._

import scala.util.Random

trait BlockDagStorageTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with EitherValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {
  val scheduler = Scheduler.fixedPool("block-dag-storage-test-scheduler", 4)

  def withDagStorage[R](f: BlockDagStorage[Task] => Task[R]): R

  val genesis = getRandomBlock(setBonds = Some(List.empty))

  "DAG Storage" should "be able to lookup a stored block" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      withDagStorage { dagStorage =>
        for {
          _   <- blockElements.traverse_(dagStorage.insert(_, false))
          dag <- dagStorage.getRepresentation
          blockElementLookups <- blockElements.traverse { b =>
                                  for {
                                    blockMetadata     <- dag.lookup(b.blockHash)
                                    latestMessageHash <- dag.latestMessageHash(b.sender)
                                    latestMessage     <- dag.latestMessage(b.sender)
                                  } yield (blockMetadata, latestMessageHash, latestMessage)
                                }
          latestMessageHashes <- dag.latestMessageHashes
          latestMessages      <- dag.latestMessages
          _ = blockElementLookups.zip(blockElements).foreach {
            case ((blockMetadata, latestMessageHash, latestMessage), b) =>
              blockMetadata shouldBe Some(BlockMetadata.fromBlock(b, false))
              latestMessageHash shouldBe Some(b.blockHash)
              latestMessage shouldBe Some(BlockMetadata.fromBlock(b, false))
          }
          _      = latestMessageHashes.size shouldBe blockElements.size
          result = latestMessages.size shouldBe blockElements.size
        } yield ()
      }
    }
  }

  it should "be able to handle checking if contains a block with empty hash" in {
    withDagStorage { dagStorage =>
      for {
        dag        <- dagStorage.getRepresentation
        ifContains <- dag.contains(ByteString.EMPTY)
      } yield ifContains shouldBe false
    }
  }
}
