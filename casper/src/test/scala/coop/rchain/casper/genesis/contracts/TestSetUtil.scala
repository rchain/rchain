package coop.rchain.casper.genesis.contracts

import java.nio.file.Paths

import coop.rchain.casper.protocol.{Deploy, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.collection.ListOps
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.unittest.TestSet
import coop.rchain.shared.StoreType.InMem
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.io.Source

object TestSetUtil {

  def runtime(implicit scheduler: Scheduler): Runtime = {
    val runtime = Runtime.create(Paths.get("/not/a/path"), -1, InMem)
    runtime.injectEmptyRegistryRoot[Task].unsafeRunSync
    runtime
  }

  def evalDeploy(deploy: Deploy, runtime: Runtime)(implicit scheduler: Scheduler): Unit = {
    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy.getRaw))
    )
    runtime.reducer.inj(deploy.getTerm).unsafeRunSync
  }

  def evalTerm(
      term: Par,
      runtime: Runtime
  )(implicit scheduler: Scheduler, rand: Blake2b512Random): Unit = {
    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    runtime.reducer.inj(term).unsafeRunSync
  }

  def eval(
      code: String,
      runtime: Runtime
  )(implicit scheduler: Scheduler, rand: Blake2b512Random): Unit =
    mkTerm(code) match {
      case Right(term) => evalTerm(term, runtime)
      case Left(ex)    => throw ex
    }

  def runTestsWithDeploys(tests: CompiledRholangSource, otherLibs: Seq[Deploy], runtime: Runtime)(
      implicit scheduler: Scheduler
  ): Unit = {
    val rand = Blake2b512Random(128)
    evalDeploy(StandardDeploys.listOps, runtime)(implicitly)
    eval(TestSet.code, runtime)(implicitly, rand.splitShort(0))
    otherLibs.foreach(evalDeploy(_, runtime))
    eval(tests.code, runtime)(implicitly, rand.splitShort(1))
  }

  def runTests(
      tests: CompiledRholangSource,
      otherLibs: Seq[CompiledRholangSource],
      runtime: Runtime
  )(implicit scheduler: Scheduler): Unit = {
    //load "libraries" required for all tests
    val rand = Blake2b512Random(128)
    evalDeploy(StandardDeploys.listOps, runtime)(implicitly)
    eval(TestSet.code, runtime)(implicitly, rand.splitShort(1))

    //load "libraries" required for this particular set of tests
    otherLibs.zipWithIndex.foreach {
      case (lib, idx) =>
        eval(lib.code, runtime)(implicitly, rand.splitShort((idx + 2).toShort))
    }

    eval(tests.code, runtime)(implicitly, rand.splitShort((otherLibs.length + 2).toShort))
  }

  /**
    * Extracts the descriptions of all the tests defined in the source file
    */
  def getTests(src: String): Iterator[String] =
    Source.fromFile(src).getLines().sliding(2).collect {
      case Seq(line1, line2) if line1.contains("TestSet\"!") =>
        line2.trim.dropRight(1)
    }

  def testPassed(test: String, tuplespace: String): Boolean =
    tuplespace.contains(s"@{$test}!(true)")
}
