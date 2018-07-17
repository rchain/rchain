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

  "captureResult" should "return the value at the specified channel after a rholang computation" in {
    val purseValue = "37"
    val deploys = Seq(
      NonNegativeNumber.term,
      InterpreterUtil.mkTerm(s""" @"NonNegativeNumber"!($purseValue, "nn") """).right.get
    ).map(ProtoUtil.termDeploy(_))

    val Right(checkpoint) = runtimeManager.computeState(runtimeManager.initStateHash, deploys)
    val hash              = ByteString.copyFrom(checkpoint.root.bytes.toArray)
    val result = runtimeManager.captureResults(
      hash,
      InterpreterUtil
        .mkTerm(""" for(@nn <- @"nn"){ @[nn, "value"]!("__PURSEVALUE__") } """)
        .right
        .get,
      "__PURSEVALUE__")

    result.size should be(1)
    result.head should be(InterpreterUtil.mkTerm(purseValue).right.get)
  }

  it should "handle multiple results and no results appropriately" in {
    val n           = 8
    val code        = (1 to n).map(i => s""" @"__SCALA__"!($i) """).mkString("|")
    val term        = InterpreterUtil.mkTerm(code).right.get
    val manyResults = runtimeManager.captureResults(runtimeManager.initStateHash, term)
    val noResults =
      runtimeManager.captureResults(runtimeManager.initStateHash, term, "differentName")

    noResults.isEmpty should be(true)

    manyResults.size should be(n)
    (1 to n).forall(i => manyResults.contains(InterpreterUtil.mkTerm(i.toString).right.get)) should be(
      true)
  }
}
