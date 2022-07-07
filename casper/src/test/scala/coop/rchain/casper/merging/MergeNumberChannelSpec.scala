package coop.rchain.casper.merging

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.rholang.Resources
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.EventConverter
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Span
import coop.rchain.models.{GPrivate, Par}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.RhoType.{Name, Number}
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic.convertToReadNumber
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.HotStoreTrieAction
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsDiff
import coop.rchain.rspace.merger.{
  ChannelChange,
  EventLogMergingLogic,
  StateChange,
  StateChangeMerger
}
import coop.rchain.rspace.serializers.ScodecSerialize
import coop.rchain.rspace.syntax._
import coop.rchain.sdk.dag.merging.DagMergingLogic
import coop.rchain.sdk.dag.merging.DagMergingLogic._
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import scodec.bits.ByteVector

final case class DeployTestInfo(term: String, cost: Long, sig: String)

class MergeNumberChannelSpec extends AnyFlatSpec {

  val rhoST = """
                |new MergeableTag, stCh  in {
                |  @(*MergeableTag, *stCh)!(0) |
                |
                |  contract @"SET"(ret, @v) = {
                |    for(@s <- @(*MergeableTag, *stCh)) {
                |      @(*MergeableTag, *stCh)!(s + v) | ret!(s, s + v)
                |    }
                |  } |
                |
                |  contract @"READ"(ret) = {
                |    for(@s <<- @(*MergeableTag, *stCh)) {
                |      ret!(s)
                |    }
                |  }
                |}
                |""".stripMargin

  def rhoChange(num: Long) = s"""
                            |new retCh, out(`rho:io:stdout`) in {
                            |  out!(("Begin change", $num)) |
                            |  @"SET"!(*retCh, $num) |
                            |  for(@old, @new_ <- retCh) {
                            |    out!(("Changed", old, "=>", new_))
                            |  }
                            |}
                            |""".stripMargin

  val rhoRead = """
                  |new retCh, out(`rho:io:stdout`) in {
                  |  @"READ"!(*retCh) |
                  |  for(@s <- retCh) {
                  |    out!(("Read st:", s))
                  |  }
                  |}
                  |""".stripMargin

  val rhoExploreRead = """
                         |new return in {
                         |  @"READ"!(*return)
                         |}
                         |""".stripMargin

  def parRho(ori: String, appendRho: String) = Seq(ori, appendRho).mkString("|")

  def makeSig(hex: String) = {
    val bv = ByteVector.fromHex(hex).get
    ByteString.copyFrom(bv.toArray)
  }

  def baseRhoSeed: Blake2b512Random = {
    val bytes: Array[Byte] = Array.fill(128)(1)
    Blake2b512Random(bytes)
  }

  val unforgeableNameSeed: Par = {
    Name(baseRhoSeed.next())
  }

  def testCase[F[_]: Concurrent: ContextShift: Parallel: Span: Log](
      baseTerms: Seq[String],
      leftTerms: Seq[DeployTestInfo],
      rightTerms: Seq[DeployTestInfo],
      expectedRejected: Set[ByteString],
      expectedFinalResult: Long
  ) = {

    Resources.mkRuntimeManager[F]("merging-test", unforgeableNameSeed).use { rm =>
      for {
        runtime <- rm.spawnRuntime

        // Run Rholang terms / simulate deploys in a block
        runRholang = (terms: Seq[DeployTestInfo], preState: Blake2b256Hash) =>
          for {
            _ <- runtime.reset(preState)

            evalResults <- terms.toList.traverse {
                            case deploy =>
                              for {
                                evalResult <- runtime.evaluate(deploy.term)
                                _ = assert(
                                  evalResult.errors.isEmpty,
                                  s"${evalResult.errors}\n ${deploy.term}"
                                )
                                // Get final values for mergeable (number) channels
                                numChanFinal <- runtime
                                                 .getNumberChannelsData(evalResult.mergeable)

                                softPoint <- runtime.createSoftCheckpoint
                              } yield (softPoint, numChanFinal)
                          }
            // Create checkpoint with state hash
            endCheckpoint <- runtime.createCheckpoint

            (logSeq, numChanAbs) = evalResults.unzip

            numChanDiffs <- rm.convertNumberChannelsToDiff(numChanAbs, preState)

            // Create event log indices
            evLogIndices <- logSeq.zip(numChanDiffs).zip(terms).traverse {
                             case ((cp, numberChanDiff), deploy) =>
                               for {
                                 evLogIndex <- BlockIndex.createEventLogIndex(
                                                cp.log
                                                  .map(EventConverter.toCasperEvent)
                                                  .toList,
                                                rm.getHistoryRepo,
                                                preState,
                                                numberChanDiff
                                              )
                                 sigBS = makeSig(deploy.sig)
                               } yield DeployIndex(sigBS, deploy.cost, evLogIndex)
                           }
          } yield (evLogIndices.toSet, endCheckpoint.root)

        historyRepo = rm.getHistoryRepo

        // Base state
        _ <- baseTerms.zipWithIndex.toList.traverse {
              case (term, i) =>
                for {
                  baseRes <- runtime
                              .evaluate(term, Cost.UNSAFE_MAX, Map.empty[String, Par], baseRhoSeed)
                  _ = assert(baseRes.errors.isEmpty, s"BASE $i: ${baseRes.errors}")
                } yield ()
            }
        baseCp <- runtime.createCheckpoint

        // Branch 1 change
        leftResult                     <- runRholang(leftTerms, baseCp.root)
        (leftEvIndices, leftPostState) = leftResult

        leftDeployIndices = DagMergingLogic.computeGreedyNonIntersectingBranches[DeployIndex](
          leftEvIndices,
          (x, y) => EventLogMergingLogic.depends(x.eventLogIndex, y.eventLogIndex)
        )

        // Branch 2 change
        rightResult                      <- runRholang(rightTerms, baseCp.root)
        (rightEvIndices, rightPostState) = rightResult

        rightDeployIndices = DagMergingLogic.computeGreedyNonIntersectingBranches[DeployIndex](
          rightEvIndices,
          (x, y) => EventLogMergingLogic.depends(x.eventLogIndex, y.eventLogIndex)
        )

        // Calculate deploy chains / deploy dependency

        leftDeployChains <- leftDeployIndices.toList.traverse(
                             DeployChainIndex(_, baseCp.root, leftPostState, historyRepo)
                           )
        rightDeployChains <- rightDeployIndices.toList.traverse(
                              DeployChainIndex(_, baseCp.root, rightPostState, historyRepo)
                            )

        _ = println(s"DEPLOY_CHAINS LEFT : ${leftDeployChains.size}")
        _ = println(s"DEPLOY_CHAINS RIGHT: ${rightDeployChains.size}")

        // Detect rejections / number channel overflow/negative
        // Base state reader
        baseReader       <- rm.getHistoryRepo.getHistoryReader(baseCp.root)
        baseReaderBinary = baseReader.readerBinary
        baseGetData      = baseReader.getData _

        // Merging handler for number channels
        overrideTrieAction = (
            hash: Blake2b256Hash,
            changes: ChannelChange[ByteVector],
            numberChs: NumberChannelsDiff
        ) =>
          numberChs.get(hash).traverse {
            RholangMergingLogic.calculateNumberChannelMerge(hash, _, changes, baseGetData)
          }

        // Create store actions / uses handler for number channels
        computeTrieActions = (changes: StateChange, mergeableChs: NumberChannelsDiff) => {
          StateChangeMerger
            .computeTrieActions(changes, baseReaderBinary, mergeableChs, overrideTrieAction)
        }

        applyTrieActions = (actions: Seq[HotStoreTrieAction]) =>
          rm.getHistoryRepo.reset(baseCp.root).flatMap(_.doCheckpoint(actions).map(_.root))

        actualSet = leftDeployChains ++ rightDeployChains
        baseMergeableChRes <- actualSet
                               .map(_.eventLogIndex.numberChannelsData)
                               .flatMap(_.keys)
                               .toList
                               .traverse(
                                 channelHash =>
                                   convertToReadNumber(baseGetData)
                                     .apply(channelHash)
                                     .map(res => (channelHash, res.getOrElse(0L)))
                               )
                               .map(_.toMap)

        dependencyMap = computeDependencyMap(
          actualSet.toSet,
          actualSet.toSet,
          DeployChainIndex.isDependency
        )
        conflictsMap = computeConflictsMap(
          actualSet.toSet,
          actualSet.toSet,
          DeployChainIndex.deploysAreConflicting
        )
        mergeableDiffs = (leftDeployChains ++ rightDeployChains)
          .map(d => d -> d.eventLogIndex.numberChannelsData)
          .toMap

        rejected = DagMergingLogic.resolveConflictSet[DeployChainIndex, Blake2b256Hash](
          conflictSet = actualSet.toSet,
          dependencyMap = dependencyMap,
          conflictsMap = conflictsMap,
          cost = DeployChainIndex.deployChainCost,
          mergeableDiffs = mergeableDiffs,
          baseMergeableChRes
        )
        toMerge = actualSet.toSet -- rejected

        allChanges = toMerge.toList.map(_.stateChanges).combineAll

        // All number channels merged
        // TODO: Negative or overflow should be rejected before!
        allMergeableChannels = toMerge.toList
          .map(_.eventLogIndex.numberChannelsData)
          .combineAll

        trieActions  <- computeTrieActions(allChanges, allMergeableChannels)
        finalHash    <- applyTrieActions(trieActions)
        rejectedSigs = rejected.flatMap(_.deploysWithCost.map(_.id))

        _ = rejectedSigs shouldBe expectedRejected

        // Read merged value

        res <- runtime.playExploratoryDeploy(rhoExploreRead, finalHash.toByteString)

        Number(finalBalance) = res.head

        _ = finalBalance shouldBe expectedFinalResult

      } yield ()
    }
  }
  implicit val timeEff = new LogicalTime[Task]
  implicit val logEff  = Log.log[Task]
  implicit val spanEff = Span.noop[Task]

  "multiple branches" should "reject deploy when mergeable number channels got negative number" in effectTest {
    testCase[Task](
      baseTerms = Seq(rhoST, rhoChange(10)),
      leftTerms = Seq(
        DeployTestInfo(rhoChange(-5), 10L, "0x11") //  -5
      ),
      rightTerms = Seq(
        DeployTestInfo(rhoChange(-6), 10L, "0x22") // -20
      ),
      expectedRejected = Set(makeSig("0x22")),
      expectedFinalResult = 5
    )
  }

  "multiple branches" should "reject deploy when mergeable number channels got overflow" in effectTest {
    testCase[Task](
      baseTerms = Seq(rhoST, rhoChange(10)),
      leftTerms = Seq(
        DeployTestInfo(rhoChange(-5), 10L, "0x11") //  -5
      ),
      rightTerms = Seq(
        DeployTestInfo(rhoChange(9223372036854775806L), 10L, "0x22") // + 9223372036854775802, reject this one
      ),
      expectedRejected = Set(makeSig("0x22")),
      expectedFinalResult = 5
    )
  }

  "multiple branches with normal rejection" should "choose from normal reject options" in effectTest {
    testCase[Task](
      baseTerms = Seq(rhoST, rhoChange(100)),
      leftTerms = Seq(
        DeployTestInfo(parRho(rhoChange(-20), "@\"X\"!(1)"), 10L, "0x11"),
        DeployTestInfo(rhoChange(-10), 10L, "0x12")
      ),
      rightTerms = Seq(
        DeployTestInfo(rhoChange(-60), 10L, "0x22"),
        DeployTestInfo(parRho(rhoChange(-20), "for(_ <- @\"X\") {Nil}"), 11L, "0x21")
      ),
      expectedRejected = Set(makeSig("0x11")),
      expectedFinalResult = 10
    )
  }

  "multiple branches" should "merge number channels" in effectTest {
    testCase[Task](
      baseTerms = Seq(rhoST),
      leftTerms = Seq(
        DeployTestInfo(rhoChange(10), 10L, "0x10"),
        DeployTestInfo(rhoChange(-5), 10L, "0x11")
      ),
      rightTerms = Seq(
        DeployTestInfo(rhoChange(15), 10L, "0x20"), // +15
        DeployTestInfo(rhoChange(10), 10L, "0x21"), // +10
        DeployTestInfo(rhoChange(-20), 10L, "0x22") // -20
      ),
      expectedRejected = Set(makeSig("0x11")), // TODO make mergeable deploys depending, this should be empty
      expectedFinalResult = 15
    )
  }

  "TEMP encode multiple values" should "show stored binary size" in {
    val rnd = Blake2b512Random.defaultRandom

    val (res, _) = (1L to 10L).foldLeft((Vector[ByteVector](), rnd)) {
      case ((acc, r), n) =>
        val ch      = Blake2b256Hash.create(Array[Byte](n.toByte))
        val encoded = RholangMergingLogic.createDatumEncoded(ch, n, r)
        val newAcc  = acc :+ encoded

        newAcc -> r.splitByte(n.toByte)
    }

    val total = ScodecSerialize.encodeDatumsBinary(res)

    println(s"Values ${res.size}, encoded bytes: ${total.size}")
  }
}
