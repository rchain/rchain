package coop.rchain.node.mergeablity

import cats.effect.{Concurrent, Sync}
import cats.implicits.catsSyntaxApplicative
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.node.revvaultexport.RhoTrieTraverser
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

final case class KeyValue(key: String, value: String)
final case class UpdatingKeyValue(key: String, oriValue: String, updatingValue: String) {
  def originKeyValue: KeyValue  = KeyValue(key, oriValue)
  def updatedKeyValue: KeyValue = KeyValue(key, updatingValue)
}

/**
  *  treeHashMap structure is like below
  *                 root  (depth=1)
  *                /     \
  *               3      10           nybble node
  *              /         \
  *             1           2         nybble node
  *      {suffix: value}   {suffix: value}
  *
  * Terminology:
  * 1. NYBBLE NODE -> 3, 10 are nybble node
  * 2. SUFFIX VALUE ->  leaf value in the bottom like {suffix: value}
  *
  * The available operations in treeHashMap is `get`, `fastUnsafeGet`, `set`, `contains`, `update`.
  * Be aware that the conflict happens in the treeHashMap only when
  * 1. the common NYBBLE NODEs are modified in the same time.
  * 2. the same SUFFIX VALUE are modified in the same time.
  *
  * Because of that, it is only possible that `set` and `update` would cause conflict while it is also possible
  * `set` and `update` would be mergeable when they don't modified the common NYBBLE NODEs or the same SUFFIX VALUE
  */
class TreeHashMapMergeabilitySpec
    extends FlatSpec
    with GeneratorDrivenPropertyChecks
    with ComputeMerge {

  private val keyValuesGen: Gen[KeyValue] = for {
    keyArb   <- Gen.alphaStr
    valueArb <- Gen.alphaStr
  } yield KeyValue(keyArb, valueArb)

  private val updatingKeyValueGen: Gen[UpdatingKeyValue] = for {
    keyArb      <- Gen.alphaStr
    valueArb    <- Gen.alphaStr
    updatingArb <- Gen.alphaStr
  } yield UpdatingKeyValue(keyArb, valueArb, updatingArb)

  private val listKeyValuesGen: Gen[List[KeyValue]] = Gen.nonEmptyListOf(keyValuesGen) suchThat (
      // make sure keys are not duplicate
      kvs => kvs.map(_.key).distinct.length == kvs.length
  )

  private val listKeyValuesUpdateGen: Gen[List[UpdatingKeyValue]] =
    Gen.nonEmptyListOf(updatingKeyValueGen) suchThat (
        // make sure keys are not duplicate
        kvs => kvs.map(_.key).distinct.length == kvs.length
    )

  private val SHARD_ID = "root-shard"

  /**
    * This is a mergeable example with depth=1 which would get 2 nybble nodes.
    *
    * Base:
    *                      rootNode
    *                  /            \
    *               3                4
    *              /                   \
    *             10                    2
    *             |                     |
    *   {"XspL2n7hsd":3}      {"g6smo8DVcr":4}
    *
    * Left:
    *
    *                           rootNode
    *                    /                       \
    *                  3 (Change exist node)       4
    *               /      \                         \
    *             10        11(Add new node)          2
    *            |            |                       |
    *    {"XspL2n7hsd":3}   {"VbqWeRbRoT":1}      {"g6smo8DVcr": 4}
    *
    *
    * Right:
    *                           rootNode
    *                     /                \
    *                  3              4(Changed exist node)
    *               /                      /               \
    *             10                     2                  3(Add new node)
    *            |                       |                  |
    *    {"XspL2n7hsd":3}       {"g6smo8DVcr":4}  {"VNYuuWj1mH": 2}
    *
    * Because left and right are not touching the same channel, they are mergeable.
    *
    * Merged:
    *                                    rootNode
    *                        /                             \
    *                  3 (Change exist node)               4(Change exist node)
    *               /      \                           /              \
    *             10        11(Add new node)          2              3(Add new node)
    *            |            |                      |                   \
    *    {"XspL2n7hsd":3}   {"VbqWeRbRoT":1}   {"g6smo8DVcr":4}   {"VNYuuWj1mH": 2}
    *
    */
  it should "write and write on the same treeHashMap without changing nybble nodes is mergeable" in {
    TreeHashMapMergeableTestCase(
      left = writeTrieRho(List(KeyValue("VbqWeRbRoT", "1"))),
      right = writeTrieRho(List(KeyValue("VNYuuWj1mH", "2"))),
      base = trieInitializedRho(
        depth = 1,
        List(KeyValue("XspL2n7hsd", "3"), KeyValue("g6smo8DVcr", "4"))
      ),
      expectedKeyValue = List(
        KeyValue("VbqWeRbRoT", "1"),
        KeyValue("VNYuuWj1mH", "2"),
        KeyValue("XspL2n7hsd", "3"),
        KeyValue("g6smo8DVcr", "4")
      ),
      treeHashMapDepth = 1
    )
  }

  /**
    * This is a conflict example with depth=1 which would get 2 nybble nodes.
    * This example modified the common node which cause conflict.
    *
    * Base:
    *                      rootNode
    *                  /            \
    *               3                4
    *              /                   \
    *             10                    2
    *             |                     |
    *   {"XspL2n7hsd":3}      {"g6smo8DVcr":4}
    *
    * Left:
    *
    *                           rootNode
    *                    /                       \
    *                  3 (Change exist node)       4
    *               /      \                         \
    *             10        11(Add new node)          2
    *            |            |                       |
    *    {"XspL2n7hsd":3}   {"VbqWeRbRoT":1}      {"g6smo8DVcr": 4}
    *
    * Right:
    *                          rootNode
    *                       /            \
    *                  3(Change exist node)   4
    *              /           \               \
    *        9(Add new node)    10               2
    *          |               |                  |
    *   {"YPaovJ0nB0":2}   {"XspL2n7hsd":3}      {"g6smo8DVcr":4}
    *
    * Because left and right are changing the same nybble node 3, they are conflict.
    *
    * Merged(reject right):
    *                           rootNode
    *                    /                       \
    *                  3 (Change exist node)       4
    *               /      \                         \
    *             10        11(Add new node)          2
    *            |            |                       |
    *    {"XspL2n7hsd":3}   {"VbqWeRbRoT":1}      {"g6smo8DVcr": 4}
    *
    */
  it should "write and write on the same treeHashMap changing the same nybble node is conflict" in {
    TreeHashMapConflictTestCase(
      left = writeTrieRho(List(KeyValue("VbqWeRbRoT", "1"))),
      right = writeTrieRho(List(KeyValue("YPaovJ0nB0", "2"))),
      base = trieInitializedRho(
        depth = 1,
        List(KeyValue("XspL2n7hsd", "3"), KeyValue("g6smo8DVcr", "4"))
      ),
      rejectRight = true,
      expectedKeyValue = List(
        KeyValue("VbqWeRbRoT", "1"),
        KeyValue("XspL2n7hsd", "3"),
        KeyValue("g6smo8DVcr", "4")
      ),
      treeHashMapDepth = 1
    )
  }

  /**
    * This is a conflict example with depth=1 which would get 2 nybble nodes.
    * This example modified the same suffix value which cause conflict.
    *
    * Base:
    *                      rootNode
    *                  /            \
    *               3                4
    *              /                   \
    *             10                    2
    *             |                     |
    *   {"XspL2n7hsd":3}      {"g6smo8DVcr":4}
    *
    * Left:
    *
    *                           rootNode
    *                  /                  \
    *               3                       4
    *              /                        \
    *             10                          2
    *             |                             |
    *   {"XspL2n7hsd":3                 {"g6smo8DVcr":4}
    *    "wce0v7V8BF":1(insert new)}
    *
    * Right:
    *                           rootNode
    *                  /                  \
    *               3                       4
    *              /                        \
    *             10                          2
    *             |                             |
    *   {"XspL2n7hsd":3                 {"g6smo8DVcr":4}
    *    "NWYic92Xb2":2(insert new)}
    *
    * Because left and right are changing the same suffix value root-3-10-suffix, they are conflict.
    *
    * Merged(reject right):
    *                           rootNode
    *                  /                  \
    *               3                       4
    *              /                        \
    *             10                          2
    *             |                             |
    *   {"XspL2n7hsd":3                 {"g6smo8DVcr":4}
    *    "wce0v7V8BF":1(insert new)}
    *
    */
  it should "write and write on the same treeHashMap changing the same suffix value is conflict" in {
    TreeHashMapConflictTestCase(
      left = writeTrieRho(List(KeyValue("wce0v7V8BF", "1"))),
      right = writeTrieRho(List(KeyValue("NWYic92Xb2", "2"))),
      base = trieInitializedRho(
        depth = 1,
        List(KeyValue("XspL2n7hsd", "3"), KeyValue("g6smo8DVcr", "4"))
      ),
      rejectRight = true,
      expectedKeyValue = List(
        KeyValue("wce0v7V8BF", "1"),
        KeyValue("XspL2n7hsd", "3"),
        KeyValue("g6smo8DVcr", "4")
      ),
      treeHashMapDepth = 1
    )
  }

  it should "read on the same treeHashMap is mergeable" in forAll(
    listKeyValuesGen,
    // rholang treeHashMap depth range based on https://github.com/rchain/rchain/blob/dev/casper/src/main/resources/Registry.rho#L60
    Gen.chooseNum(0, 16),
    minSuccessful(2)
  ) { (initialKVS, depth) =>
    TreeHashMapMergeableTestCase(
      readTrieRho(initialKVS.map(_.key)),
      readTrieRho(initialKVS.map(_.key)),
      trieInitializedRho(depth, initialKVS),
      initialKVS,
      depth
    )
  }

  it should "read and write on different keys on the same treeHashMap is mergeable" in forAll(
    listKeyValuesGen,
    listKeyValuesGen,
    Gen.chooseNum(0, 16),
    minSuccessful(3)
  ) { (initialKVS, writeKeyValues, depth) =>
    whenever((initialKVS.map(_.key).toSet intersect writeKeyValues.map(_.key).toSet).isEmpty) {
      val expectedKeyValues = (initialKVS ++ writeKeyValues)
        .foldLeft(Map.empty[String, String]) {
          case (acc, kv) => acc.updated(kv.key, kv.value)
        }
        .toList
        .map(KeyValue.tupled)
      TreeHashMapMergeableTestCase(
        readTrieRho(initialKVS.map(_.key)),
        writeTrieRho(writeKeyValues),
        trieInitializedRho(depth, initialKVS),
        expectedKeyValues,
        depth
      )
    }

  }

  it should "read and update with the same keys on the same initialized treeHashMap is mergeable" in forAll(
    listKeyValuesUpdateGen,
    Gen.choose(0, 16),
    minSuccessful(3)
  ) { (keyValues, depth) =>
    TreeHashMapMergeableTestCase(
      readTrieRho(keyValues.map(_.key)),
      updateTrieRho(keyValues.map(_.updatedKeyValue)),
      trieInitializedRho(depth, keyValues.map(_.originKeyValue)),
      keyValues.map(_.updatedKeyValue),
      depth
    )
  }
  it should "read and contain with the same keys on the same initialized treeHashMap is mergeable" in forAll(
    listKeyValuesGen,
    Gen.choose(0, 16),
    minSuccessful(2)
  ) { (keyValues, depth) =>
    TreeHashMapMergeableTestCase(
      readTrieRho(keyValues.map(_.key)),
      containTrieRho(keyValues.map(_.key)),
      trieInitializedRho(depth, keyValues),
      keyValues,
      depth
    )
  }
  private def readKeyRho(keys: List[String]): String =
    keys.map(key => s"""new a in {TreeHashMap!("get", treeMap, "${key}", *a)}""").mkString("|\n")

  private def writeKeyValueRho(keyValues: List[KeyValue]): String =
    keyValues
      .map(kv => s"""new a in {TreeHashMap!("set", treeMap, "${kv.key}", "${kv.value}", *a)}""")
      .mkString("|\n")

  private def containKeyRho(keys: List[String]): String =
    keys
      .map(key => s"""new a in {TreeHashMap!("contains", treeMap, "${key}", *a)}""")
      .mkString("|\n")

  private def updateKeyValueRho(keyValues: List[KeyValue]): String =
    keyValues
      .map(kv => s"""new a, updateFn in {
                    #  TreeHashMap!("update", treeMap, "${kv.key}", *updateFn, *a)|
                    #  for (_, resultCh <- updateFn){ resultCh!("${kv.value}")}
                    #  }""".stripMargin('#'))
      .mkString("|\n")

  private def trieInitializedRho(depth: Int, initialKeyValues: List[KeyValue]): String =
    s"""new
       #  rl(`rho:registry:lookup`),
       #  TreeHashMapCh,
       #  treeMapStore
       #  in {
       #  rl!(`rho:lang:treeHashMap`, *TreeHashMapCh) |
       #  for (TreeHashMap <- TreeHashMapCh){
       #    TreeHashMap!("init", ${depth}, *treeMapStore) |
       #    for (@treeMap <-  treeMapStore){
       #      {${writeKeyValueRho(initialKeyValues)}}
       #      // store the treeHashMap handle in a public name which would be easier to retrieve
       #      |@"treeHashChannel"!(treeMap)
       #    }
       #  }
       #}
       #""".stripMargin('#')

  private def readTrieRho(keys: List[String]): String =
    s"""new
       #  rl(`rho:registry:lookup`),
       #  TreeHashMapCh
       #  in {
       #  rl!(`rho:lang:treeHashMap`, *TreeHashMapCh) |
       #  for (TreeHashMap <- TreeHashMapCh){
       #    for (@treeMap <<- @"treeHashChannel"){
       #      ${readKeyRho(keys)}
       #    }
       #  }
       #}""".stripMargin('#')

  private def writeTrieRho(keyValues: List[KeyValue]): String =
    s"""new
       #  rl(`rho:registry:lookup`),
       #  TreeHashMapCh
       #  in {
       #  rl!(`rho:lang:treeHashMap`, *TreeHashMapCh) |
       #  for (TreeHashMap <- TreeHashMapCh){
       #    for (@treeMap <<- @"treeHashChannel"){
       #      ${writeKeyValueRho(keyValues)}
       #    }
       #  }
       #}""".stripMargin('#')

  private def containTrieRho(keys: List[String]): String =
    s"""new
       #  rl(`rho:registry:lookup`),
       #  TreeHashMapCh
       #  in {
       #  rl!(`rho:lang:treeHashMap`, *TreeHashMapCh) |
       #  for (TreeHashMap <- TreeHashMapCh){
       #    for (@treeMap <<- @"treeHashChannel"){
       #      ${containKeyRho(keys)}
       #    }
       #  }
       #}""".stripMargin('#')

  private def updateTrieRho(keyValues: List[KeyValue]): String =
    s"""new
       #  rl(`rho:registry:lookup`),
       #  TreeHashMapCh
       #  in {
       #  rl!(`rho:lang:treeHashMap`, *TreeHashMapCh) |
       #  for (TreeHashMap <- TreeHashMapCh){
       #    for (@treeMap <<- @"treeHashChannel"){
       #      ${updateKeyValueRho(keyValues)}
       #    }
       #  }
       #}""".stripMargin('#')

  private val getTreeHashMapHandle: String = """new return in {
                               #  for (@result<- @"treeHashChannel"){
                               #    return!(result)
                               #  }
                               #}""".stripMargin('#')

  def getAllKeyValueFromTree[F[_]: Sync: Span: Log](
      runtime: RhoRuntime[F],
      depth: Int,
      stateHash: Blake2b256Hash
  ): F[Map[ByteString, String]] =
    for {
      treeMapHandleR <- runtime.playExploratoryDeploy(
                         getTreeHashMapHandle,
                         stateHash.toByteString
                       )
      treeMapHandle = treeMapHandleR.head
      maps          <- RhoTrieTraverser.traverseTrie(depth, treeMapHandle, runtime)
      result = RhoTrieTraverser.vecParMapToMap(
        maps,
        p => p.exprs.head.getGByteArray,
        p => p.exprs.head.getGString
      )
    } yield result

  def TreeHashMapMergeableTestCase(
      left: String,
      right: String,
      base: String,
      expectedKeyValue: List[KeyValue],
      treeHashMapDepth: Int
  ): Unit =
    runTest(left)(right)(base)(rejectRight = false)(isConflict = false)(
      expectedKeyValue = expectedKeyValue
    )(treeHashMapDepth = treeHashMapDepth)

  def TreeHashMapConflictTestCase(
      left: String,
      right: String,
      base: String,
      rejectRight: Boolean,
      expectedKeyValue: List[KeyValue],
      treeHashMapDepth: Int
  ): Unit =
    runTest(left)(right)(base)(rejectRight = rejectRight)(isConflict = true)(
      expectedKeyValue = expectedKeyValue
    )(treeHashMapDepth = treeHashMapDepth)

  private def runTest(left: String)(right: String)(base: String)(
      rejectRight: Boolean
  )(isConflict: Boolean)(expectedKeyValue: List[KeyValue])(treeHashMapDepth: Int): Unit = {
    implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val logger: Log[Task]         = Log.log[Task]
    val baseDeploy =
      ConstructDeploy.sourceDeploy(base, 1L, phloLimit = Cost.UNSAFE_MAX.value, shardId = SHARD_ID)
    val leftDeploy =
      ConstructDeploy.sourceDeploy(left, 2L, phloLimit = Cost.UNSAFE_MAX.value, shardId = SHARD_ID)
    val rightDeploy = ConstructDeploy.sourceDeploy(
      right,
      3L,
      phloLimit = Cost.UNSAFE_MAX.value,
      sec = ConstructDeploy.defaultSec2,
      shardId = SHARD_ID
    )
    computeMergeCase[Task](
      Seq(StandardDeploys.registry(SHARD_ID), baseDeploy),
      Seq(leftDeploy),
      Seq(rightDeploy),
      (runtime, _, mergedState) =>
        for {
          mergedTreeMap <- getAllKeyValueFromTree(runtime, depth = treeHashMapDepth, mergedState._1)
          _ <- Sync[Task]
                .raiseError(new Exception(s"Mergeable case failed with :${mergedState}"))
                .whenA(mergedState._2.nonEmpty && !isConflict)
          _ <- Sync[Task]
                .raiseError(new Exception(s"Conflict case failed with :${mergedState}"))
                .whenA(mergedState._2.isEmpty && isConflict)
          _ <- Sync[Task]
                .raiseError(
                  new Exception(s"""The mergedTreeHashMap length is not equal to expectedKeyValue.
                 # MergedTreeHashMap: ${mergedTreeMap}
                 # ExpectedKeyValues: ${expectedKeyValue}""".stripMargin('#'))
                )
                .whenA(mergedTreeMap.toList.length != expectedKeyValue.length)
          _ <- expectedKeyValue.traverse(
                kv =>
                  Sync[Task]
                    .raiseError(
                      new Exception(
                        s"""The mergedTreeHashMap content is not equal to expectedKeyValue.
                           # MergedTreeHashMap: ${mergedTreeMap}
                           # ExpectedKeyValues: ${expectedKeyValue}"""
                          .stripMargin('#')
                      )
                    )
                    .whenA({
                      val hashedKey = RhoTrieTraverser
                        .keccakKey(kv.key)
                        .exprs
                        .head
                        .getGByteArray
                        .substring(treeHashMapDepth, 32)
                      mergedTreeMap.getOrElse(hashedKey, "") != kv.value
                    })
              )
        } yield (),
      rejectRight = rejectRight
    ).runSyncUnsafe()

  }

}
