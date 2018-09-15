package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.Capture.taskCapture
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.collection.ListOps
import coop.rchain.rholang.unittest.TestSet
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.shared.StoreType.InMem
import java.nio.file.Paths

import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import monix.eval.Task
import monix.execution.Scheduler

import scala.io.Source

object TestSetUtil {

  def runtime(name: String): Runtime = Runtime.create(Paths.get("/not/a/path"), -1, InMem)

  def eval_term(term: Par, runtime: Runtime)(implicit scheduler: Scheduler,
                                             rand: Blake2b512Random,
                                             costAccountingAlg: CostAccountingAlg[Task]): Unit =
    runtime.reducer.inj(term).unsafeRunSync

  def eval(code: String, runtime: Runtime)(implicit scheduler: Scheduler,
                                           rand: Blake2b512Random,
                                           costAccountingAlg: CostAccountingAlg[Task]): Unit =
    mkTerm(code) match {
      case Right(term) => eval_term(term, runtime)
      case Left(ex)    => throw ex
    }

  def runTests(tests: CompiledRholangSource,
               otherLibs: Seq[CompiledRholangSource],
               runtime: Runtime)(implicit scheduler: Scheduler): Unit = {
    //load "libraries" required for all tests
    val rand              = Blake2b512Random(128)
    val costAccountingAlg = CostAccountingAlg.unsafe[Task](CostAccount(Integer.MAX_VALUE))
    eval(ListOps.code, runtime)(implicitly, rand.splitShort(0), costAccountingAlg)
    eval(TestSet.code, runtime)(implicitly, rand.splitShort(1), costAccountingAlg)

    //load "libraries" required for this particular set of tests
    otherLibs.zipWithIndex.foreach {
      case (lib, idx) =>
        eval(lib.code, runtime)(implicitly, rand.splitShort((idx + 2).toShort), costAccountingAlg)
    }

    eval(tests.code, runtime)(implicitly,
                              rand.splitShort((otherLibs.length + 2).toShort),
                              costAccountingAlg)
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
