package coop.rchain.blockstorage.dag

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockMetadata
import coop.rchain.models.blockImplicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait BlockDagStorageTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with EitherValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {
  val scheduler = Scheduler.fixedPool("block-dag-storage-test-scheduler", 4)

  def withDagStorage[R](f: BlockDagStorage[Task] => Task[R]): R

  val genesis = getRandomBlock(
    setBonds = Some(List.empty),
    setParentsHashList = List.empty.some,
    setBlockNumber = 0L.some
  )

  "DAG Storage" should "be able to lookup a stored block" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      withDagStorage { dagStorage =>
        for {
          _   <- blockElements.traverse(dagStorage.insert(_, false))
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
          _      = latestMessageHashes.size shouldBe blockElements.size + 1
          result = latestMessages.size shouldBe blockElements.size + 1
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
