package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import coop.rchain.casper.util.ProtoUtil
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.math.NonNegativeNumber

import java.nio.file.Files

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class RuntimeManagerTest extends FlatSpec with Matchers {
  val storageSize      = 1024L * 1024
  val storageDirectory = Files.createTempDirectory("casper-runtime-manager-test")
  val activeRuntime    = Runtime.create(storageDirectory, storageSize)
  val runtimeManager   = RuntimeManager.fromRuntime(activeRuntime)

  "computeState" should "catpure rholang errors" in {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) } | @"x"!(1) | @"y"!("hi") """
    val deploy     = ProtoUtil.termDeploy(InterpreterUtil.mkTerm(badRholang).right.get)
    val result     = runtimeManager.computeState(runtimeManager.emptyStateHash, deploy :: Nil)

    result.isLeft should be(true)
  }

  "captureResult" should "return the value at the specified channel after a rholang computation" in {
    val purseValue     = "37"
    val captureChannel = "__PURSEVALUE__"
    val deploys = Seq(
      NonNegativeNumber.term,
      InterpreterUtil.mkTerm(s""" @"NonNegativeNumber"!($purseValue, "nn") """).right.get
    ).map(ProtoUtil.termDeploy(_))

    val Right((checkpoint, _)) = runtimeManager.computeState(runtimeManager.emptyStateHash, deploys)
    val hash                   = ByteString.copyFrom(checkpoint.root.bytes.toArray)
    val result = runtimeManager.captureResults(
      hash,
      InterpreterUtil
        .mkTerm(s""" for(@nn <- @"nn"){ @[nn, "value"]!("$captureChannel") } """)
        .right
        .get,
      captureChannel)

    result.size should be(1)
    result.head should be(InterpreterUtil.mkTerm(purseValue).right.get)
  }

  it should "handle multiple results and no results appropriately" in {
    val n    = 8
    val code = (1 to n).map(i => s""" @"__SCALA__"!($i) """).mkString("|")
    val term = InterpreterUtil.mkTerm(code).right.get
    val manyResults =
      runtimeManager.captureResults(runtimeManager.emptyStateHash, term, "__SCALA__")
    val noResults =
      runtimeManager.captureResults(runtimeManager.emptyStateHash, term, "differentName")

    noResults.isEmpty should be(true)

    manyResults.size should be(n)
    (1 to n).forall(i => manyResults.contains(InterpreterUtil.mkTerm(i.toString).right.get)) should be(
      true)
  }

  "emptyStateHash" should "not remember previous hot store state" in {
    val testStorageDirectory = Files.createTempDirectory("casper-runtime-manager-test")

    val testRuntime1        = Runtime.create(testStorageDirectory, storageSize)
    val testRuntimeManager1 = RuntimeManager.fromRuntime(testRuntime1)
    val hash1               = testRuntimeManager1.emptyStateHash
    val deploy              = ProtoUtil.basicDeploy(0)
    val Right(_)            = testRuntimeManager1.computeState(hash1, deploy :: Nil)
    testRuntime1.close()

    val testRuntime2        = Runtime.create(testStorageDirectory, storageSize)
    val testRuntimeManager2 = RuntimeManager.fromRuntime(testRuntime2)
    val hash2               = testRuntimeManager2.emptyStateHash
    testRuntime2.close()

    hash1 should be(hash2)
  }
}
