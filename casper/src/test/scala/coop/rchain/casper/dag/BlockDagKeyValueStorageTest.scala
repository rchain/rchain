package coop.rchain.casper.dag

import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.{BlockDagStorage, BlockDagStorageTest}
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits._
import coop.rchain.shared
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task

class BlockDagKeyValueStorageTest extends BlockDagStorageTest {

  private def createDagStorage: Task[BlockDagStorage[Task]] = {
    implicit val log     = new shared.Log.NOPLog[Task]()
    implicit val metrics = new Metrics.MetricsNOP[Task]
    val kvm              = InMemoryStoreManager[Task]
    BlockDagKeyValueStorage.create[Task](kvm)
  }

  override def withDagStorage[R](f: BlockDagStorage[Task] => Task[R]): R =
    (createDagStorage >>= { ds =>
      ds.insert(genesis, false, approved = true) >> f(ds)
    }).unsafeRunSync(scheduler)
  type LookupResult =
    (
        List[
          (
              Option[BlockMetadata],
              Option[BlockHash],
              Option[BlockMetadata],
              Option[Set[BlockHash]],
              Boolean
          )
        ],
        Map[Validator, BlockHash],
        Map[Validator, BlockMetadata],
        Vector[Vector[BlockHash]],
        Long
    )

  private def lookupElements(
      blockElements: List[BlockMessage],
      storage: BlockDagStorage[Task],
      topoSortStartBlockNumber: Long = 0
  ): Task[LookupResult] = {
    import cats.instances.list._
    for {
      dag <- storage.getRepresentation
      list <- blockElements.traverse { b =>
               for {
                 blockMetadata     <- dag.lookup(b.blockHash)
                 latestMessageHash <- dag.latestMessageHash(b.sender)
                 latestMessage     <- dag.latestMessage(b.sender)
                 children          <- dag.children(b.blockHash)
                 contains          <- dag.contains(b.blockHash)
               } yield (blockMetadata, latestMessageHash, latestMessage, children, contains)
             }
      latestMessageHashes <- dag.latestMessageHashes
      latestMessages      <- dag.latestMessages
      topoSort            <- dag.topoSort(topoSortStartBlockNumber, none)
      latestBlockNumber   <- dag.latestBlockNumber
    } yield (list, latestMessageHashes, latestMessages, topoSort, latestBlockNumber)
  }

  private def testLookupElementsResult(
      lookupResult: LookupResult,
      blockElements: List[BlockMessage]
  ): Unit = {
    val (list, latestMessageHashes, latestMessages, topoSort, latestBlockNumber) = lookupResult
    val realLatestMessages = blockElements.foldLeft(Map.empty[Validator, BlockMetadata]) {
      case (lm, b) =>
        // Ignore empty sender for genesis block
        if (b.sender != ByteString.EMPTY)
          lm.updated(b.sender, BlockMetadata.fromBlock(b, false))
        else
          lm
    }
    list.zip(blockElements).foreach {
      case ((blockMetadata, latestMessageHash, latestMessage, children, contains), b) =>
        blockMetadata shouldBe Some(BlockMetadata.fromBlock(b, false))
        latestMessageHash shouldBe realLatestMessages.get(b.sender).map(_.blockHash)
        latestMessage shouldBe realLatestMessages.get(b.sender)
        children shouldBe
          Some(
            blockElements
              .filter(_.header.parentsHashList.contains(b.blockHash))
              .map(_.blockHash)
              .toSet
          )
        contains shouldBe true
    }
    latestMessageHashes.filterNot { case (_, h) => h == genesis.blockHash } shouldBe realLatestMessages
      .mapValues(
        _.blockHash
      )
    latestMessages.filterNot { case (_, h) => h.blockHash == genesis.blockHash } shouldBe realLatestMessages

    def normalize(topoSort: Vector[Vector[BlockHash]]): Vector[Vector[BlockHash]] =
      if (topoSort.size == 1 && topoSort.head.isEmpty)
        Vector.empty
      else
        topoSort

    val realTopoSort = normalize(Vector(blockElements.map(_.blockHash).toVector))
    for ((topoSortLevel, realTopoSortLevel) <- topoSort.zipAll(
                                                realTopoSort,
                                                Vector.empty,
                                                Vector.empty
                                              )) {
      topoSortLevel.toSet.filterNot(_ == genesis.blockHash) shouldBe realTopoSortLevel.toSet
      latestBlockNumber shouldBe topoSort.length
    }

  }

  it should "be able to restore state on startup" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      withDagStorage { storage =>
        for {
          _      <- blockElements.traverse_(storage.insert(_, false))
          result <- lookupElements(blockElements, storage)
        } yield testLookupElementsResult(result, blockElements)
      }
    }
  }

  it should "be able to restore latest messages with genesis with empty sender field" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      val blockElementsWithGenesis = blockElements match {
        case x :: xs =>
          val genesis = x.copy(sender = ByteString.EMPTY)
          genesis :: xs
        case Nil =>
          Nil
      }
      withDagStorage { storage =>
        for {
          _      <- blockElementsWithGenesis.traverse_(storage.insert(_, false))
          result <- lookupElements(blockElementsWithGenesis, storage)
        } yield testLookupElementsResult(result, blockElementsWithGenesis)
      }
    }
  }

  it should "be able to restore state from the previous two instances" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { firstBlockElements =>
      forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) {
        secondBlockElements =>
          withDagStorage { storage =>
            for {
              _      <- firstBlockElements.traverse_(storage.insert(_, false))
              _      <- secondBlockElements.traverse_(storage.insert(_, false))
              result <- lookupElements(firstBlockElements ++ secondBlockElements, storage)
            } yield testLookupElementsResult(result, firstBlockElements ++ secondBlockElements)
          }
      }
    }
  }

  it should "be able to restore after squashing latest messages" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      forAll(blockWithNewHashesGen(blockElements), blockWithNewHashesGen(blockElements)) {
        (secondBlockElements, thirdBlockElements) =>
          withDagStorage { storage =>
            for {
              _      <- blockElements.traverse_(storage.insert(_, false))
              _      <- secondBlockElements.traverse_(storage.insert(_, false))
              _      <- thirdBlockElements.traverse_(storage.insert(_, false))
              result <- lookupElements(blockElements, storage)
            } yield testLookupElementsResult(
              result,
              blockElements ++ secondBlockElements ++ thirdBlockElements
            )
          }
      }
    }
  }

  it should "be able to restore invalid blocks on startup" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      withDagStorage { storage =>
        for {
          _             <- blockElements.traverse_(storage.insert(_, true))
          dag           <- storage.getRepresentation
          invalidBlocks <- dag.invalidBlocks
        } yield invalidBlocks shouldBe blockElements.map(BlockMetadata.fromBlock(_, true)).toSet
      }
    }
  }

  it should "be able to restore deploy index on startup" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      withDagStorage { storage =>
        for {
          _   <- blockElements.traverse_(storage.insert(_, true))
          dag <- storage.getRepresentation
          (deploys, blockHashes) = blockElements
            .flatMap(b => b.body.deploys.map(_ -> b.blockHash))
            .unzip
          deployLookups <- deploys.traverse(d => dag.lookupByDeployId(d.deploy.sig))
        } yield deployLookups shouldBe blockHashes.map(_.some)
      }
    }
  }

  it should "handle blocks with invalid numbers" in {
    forAll(blockElementGen(), blockElementGen()) { (genesis, block) =>
      withDagStorage { storage =>
        val invalidBlock = block.copy(
          body = block.body.copy(state = block.body.state.copy(blockNumber = 1000))
        )
        storage.insert(genesis, false) >>
          storage.insert(invalidBlock, true)
      }
    }
  }

  "recording of new directly finalized block" should "record finalized all non finalized ancestors of LFB" in
    withDagStorage { storage =>
      for {
        _ <- storage.insert(genesis, false, true)
        b1 = getRandomBlock(
          setParentsHashList = List(genesis.blockHash).some,
          setBlockNumber = 1L.some
        )
        _   <- storage.insert(b1, false)
        b2  = getRandomBlock(setParentsHashList = List(b1.blockHash).some, setBlockNumber = 2L.some)
        _   <- storage.insert(b2, false)
        b3  = getRandomBlock(setParentsHashList = List(b2.blockHash).some, setBlockNumber = 3L.some)
        _   <- storage.insert(b3, false)
        b4  = getRandomBlock(setParentsHashList = List(b3.blockHash).some, setBlockNumber = 4L.some)
        dag <- storage.insert(b4, false)

        // only genesis is finalized
        _ <- dag.lookupUnsafe(genesis.blockHash).map(_.finalized shouldBe true)
        _ <- dag.isFinalized(genesis.blockHash).map(_ shouldBe true)
        _ <- dag.isFinalized(b1.blockHash).map(_ shouldBe false)
        _ <- dag.lookupUnsafe(b1.blockHash).map(_.finalized shouldBe false)
        _ <- dag.isFinalized(b2.blockHash).map(_ shouldBe false)
        _ <- dag.lookupUnsafe(b2.blockHash).map(_.finalized shouldBe false)
        _ <- dag.isFinalized(b3.blockHash).map(_ shouldBe false)
        _ <- dag.lookupUnsafe(b3.blockHash).map(_.finalized shouldBe false)
        _ <- dag.isFinalized(b4.blockHash).map(_ shouldBe false)
        _ <- dag.lookupUnsafe(b4.blockHash).map(_.finalized shouldBe false)

        // record directly finalized block
        effectsRef <- Ref.of[Task, Set[BlockHash]](Set.empty)
        _          <- storage.recordDirectlyFinalized(b3.blockHash, effectsRef.set)
        dag        <- storage.getRepresentation

        // in mem DAG state should be correct
        _ = dag.lastFinalizedBlock shouldBe b3.blockHash
        _ <- dag.isFinalized(b1.blockHash).map(_ shouldBe true)
        _ <- dag.isFinalized(b2.blockHash).map(_ shouldBe true)
        _ <- dag.isFinalized(b3.blockHash).map(_ shouldBe true)
        _ <- dag.isFinalized(b4.blockHash).map(_ shouldBe false)

        // persisted state should be correct
        _ <- dag
              .lookupUnsafe(b1.blockHash)
              .map(v => {
                v.finalized shouldBe true
                v.directlyFinalized shouldBe false
              })
        _ <- dag
              .lookupUnsafe(b2.blockHash)
              .map(v => {
                v.finalized shouldBe true
                v.directlyFinalized shouldBe false
              })
        _ <- dag
              .lookupUnsafe(b3.blockHash)
              .map(v => {
                v.finalized shouldBe true
                v.directlyFinalized shouldBe true
              })
        _ <- dag
              .lookupUnsafe(b4.blockHash)
              .map(v => {
                v.finalized shouldBe false
                v.directlyFinalized shouldBe false
              })

        // all finalized should be in set supplied for finalization effect
        effects <- effectsRef.get
        _       = effects shouldBe Set(b1, b2, b3).map(_.blockHash)
      } yield ()
    }
}
