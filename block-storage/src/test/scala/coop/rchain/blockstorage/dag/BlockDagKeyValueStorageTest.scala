package coop.rchain.blockstorage.dag

import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits._
import coop.rchain.models.{BlockMetadata, EquivocationRecord}
import coop.rchain.shared
import coop.rchain.shared.syntax._
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

  it should "be able to restore equivocations tracker on startup" in {
    forAll(blockElementsWithParentsGen(genesis), minSize(0), sizeRange(10)) { blockElements =>
      forAll(validatorGen) { equivocator =>
        forAll(blockHashGen) { blockHash =>
          withDagStorage { storage =>
            for {
              _ <- blockElements.traverse_(storage.insert(_, false))
              record = EquivocationRecord(
                equivocator,
                0,
                Set(blockHash)
              )
              _ <- storage.accessEquivocationsTracker { tracker =>
                    tracker.insertEquivocationRecord(record)
                  }
              records <- storage.accessEquivocationsTracker(_.equivocationRecords)
              _       = records shouldBe Set(record)
              result  <- lookupElements(blockElements, storage)
            } yield testLookupElementsResult(result, blockElements)
          }
        }
      }
    }
  }

  it should "be able to modify equivocation records" in {
    forAll(validatorGen, blockHashGen, blockHashGen) { (equivocator, blockHash1, blockHash2) =>
      withDagStorage { storage =>
        val record = EquivocationRecord(equivocator, 0, Set(blockHash1))
        for {
          _ <- storage.accessEquivocationsTracker { tracker =>
                tracker.insertEquivocationRecord(record)
              }
          _ <- storage.accessEquivocationsTracker { tracker =>
                tracker.updateEquivocationRecord(record, blockHash2)
              }
          updatedRecord = EquivocationRecord(equivocator, 0, Set(blockHash1, blockHash2))
          records       <- storage.accessEquivocationsTracker(_.equivocationRecords)
          _             = records shouldBe Set(updatedRecord)
        } yield ()
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

  /**
    * Assuming recreating view of target block T.
    * Bi are seen, 0i not seen, but already in the most recent view.
    * All Os should be removed.
    *
    * O2 and O3 are high excess
    * O1 is range excess
    * O4 is unseenSender excess - some validator bonded in genesis has been waiting for a while before creating new block.
    *   And created on top of genesis without referencing all new messages.
    *
    *   03
    *   02     T
    *   L1 01 L2
    *      L0
    *   B0 B1 B2 O4
    * G
    */
  "truncate" should "remove all excess messages" in
    withDagStorage { storage =>
      def randomValidator: ByteString =
        ByteString.copyFrom(Array.fill(65)((scala.util.Random.nextInt(256) - 128).toByte))
      // v0, v1, v2 are bonded in genesis, v4 is bonded later in late message
      val Seq(v0, v1, v2, v3) = (1 to 4).map(_ => randomValidator)
      val g = genesis.copy(
        body = genesis.body.copy(
          state = genesis.body.state
            .copy(bonds = List(Bond(v0, 1), Bond(v1, 1), Bond(v2, 1), Bond(v3, 1)))
        )
      )

      for {
        _ <- storage.insert(g, false, true)
        Seq(b0, b1, b2) = Seq(v0, v1, v2).map { v =>
          getRandomBlock(
            setParentsHashList = List(g.blockHash).some,
            setJustifications = List(v0, v1, v2, v3).map(Justification(_, g.blockHash)).some,
            setBlockNumber = 1L.some,
            setValidator = v.some,
            setBonds = List(Bond(v0, 1), Bond(v1, 1), Bond(v2, 1), Bond(v3, 1)).some
          )
        }
        l0 = getRandomBlock(
          setParentsHashList = List(b0, b1, b2).map(_.blockHash).some,
          setJustifications = List(
            Justification(v0, b0.blockHash),
            Justification(v1, b1.blockHash),
            Justification(v2, b2.blockHash),
            Justification(v3, g.blockHash)
          ).some,
          setBlockNumber = 2L.some,
          setValidator = v1.some,
          setBonds = List(Bond(v0, 1), Bond(v1, 1), Bond(v2, 1), Bond(v3, 1)).some
        )
        Seq(l1, o1, l2) = Seq(v0, v1, v2).map { v =>
          getRandomBlock(
            setParentsHashList = List(l0.blockHash).some,
            setJustifications = List(
              Justification(v0, b0.blockHash),
              Justification(v1, l0.blockHash),
              Justification(v2, b2.blockHash),
              Justification(v3, g.blockHash)
            ).some,
            setBlockNumber = 3L.some,
            setValidator = v.some,
            setBonds = List(Bond(v0, 1), Bond(v1, 1), Bond(v2, 1), Bond(v3, 1)).some
          )
        }
        o2 = getRandomBlock(
          setParentsHashList = List(l1, o1, l2).map(_.blockHash).some,
          setBlockNumber = 4L.some,
          setJustifications = List(
            Justification(v0, l1.blockHash),
            Justification(v1, o1.blockHash),
            Justification(v2, l2.blockHash),
            Justification(v3, g.blockHash)
          ).some,
          setValidator = v0.some,
          setBonds = List(Bond(v0, 1), Bond(v1, 1), Bond(v2, 1), Bond(v3, 1)).some
        )
        o3 = getRandomBlock(
          setParentsHashList = List(o2.blockHash).some,
          setJustifications = List(
            Justification(v0, o2.blockHash),
            Justification(v1, o1.blockHash),
            Justification(v2, l2.blockHash),
            Justification(v3, g.blockHash)
          ).some,
          setBlockNumber = 5L.some,
          setValidator = v0.some,
          setBonds = List(Bond(v0, 1), Bond(v1, 1), Bond(v2, 1), Bond(v3, 1)).some
        )
        // o4 is late and building on top of genesis. So it is below range of target latest messages
        // and should be deleted
        o4 = getRandomBlock(
          setParentsHashList = List(g.blockHash).some,
          setJustifications = List(v0, v1, v2, v3).map(Justification(_, g.blockHash)).some,
          setBlockNumber = 1L.some,
          setValidator = v3.some,
          setBonds = List(Bond(v0, 1), Bond(v1, 1), Bond(v2, 1), Bond(v3, 1)).some
        )
        fullDag <- List(b0, b1, b2, l0, l1, o1, l2, o2, o3, o4)
                    .traverse(storage.insert(_, false))
                    .map(_.last)
        excess = Vector(o1, o2, o3, o4).map(_.blockHash)
        inView = Vector(b0, b1, b2, l0, l1, l2, g).map(_.blockHash)

        // all blocks should be present in full DAG
        _ <- (excess ++ inView).findM(fullDag.contains(_).not) shouldBeF None

        findLfb = (_: Map[Validator, BlockHash]) => genesis.blockHash.pure[Task]

        tDag <- fullDag.truncate(
                 List(
                   (v0, l1.blockHash),
                   (v1, l0.blockHash),
                   (v2, l2.blockHash),
                   (v3, genesis.blockHash)
                 ).toMap,
                 findLfb
               )
        // all these should be absent in truncated DAG
        _ <- excess.findM(tDag.contains) shouldBeF None
        // all these should be present in truncated DAG
        _ <- inView.findM(tDag.contains(_).not) shouldBeF None
      } yield ()
    }
}
