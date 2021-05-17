package coop.rchain.casper.util.rholang

import cats.effect.Concurrent
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.helper.TestNode.Effect
import org.scalatest.FlatSpec
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.util.Random

class RhoTrieTraverserTest extends FlatSpec {
  "traverse the TreeHashMap" should "work" in {
    val total     = 100
    val trieDepth = 2
    val insertKeyValues = (0 to total).map(
      i => (Random.alphanumeric.take(10).foldLeft("")(_ + _), Random.nextInt(1000000), i)
    )
    val insertRho = insertKeyValues.foldLeft("") {
      case (acc, (key, value, index)) =>
        if (index != total)
          acc + s"""new a in {TreeHashMap!("set", treeMap, "${key}", ${value}, *a)}|\n"""
        else acc + s"""new a in {TreeHashMap!("set", treeMap, "${key}", ${value}, *a)}\n"""
    }
    val trieInitializedRho =
      s"""
        |new
        |  rl(`rho:registry:lookup`),
        |  TreeHashMapCh,
        |  newTreeMapStore,
        |  vaultMapStore
        |  in {
        |  rl!(`rho:lang:treeHashMap`, *TreeHashMapCh) |
        |  for (TreeHashMap <- TreeHashMapCh){
        |    TreeHashMap!("init", ${trieDepth}, *vaultMapStore) |
        |    for (@treeMap <-  vaultMapStore){
        |      ${insertRho}
        |      |@"t"!(treeMap)
        |    }
        |  }
        |}
        |""".stripMargin

    val getTrieMapHandleRho = """new s in {
                               |  for (@result<- @"t"){
                               |    s!(result)
                               |  }
                               |}""".stripMargin

    implicit val concurent                   = Concurrent[Task]
    implicit val metricsEff: Metrics[Effect] = new Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Effect]      = NoopSpan[Task]()
    implicit val logger: Log[Effect]         = Log.log[Task]
    val t = rhoRuntimeEff[Effect](false).use {
      case (runtime, _, _) =>
        for {
          hash1 <- runtime.emptyStateHash
          _     <- runtime.reset(Blake2b256Hash.fromByteString(hash1))
          rd    <- runtime.processDeploy(StandardDeploys.registry)
          check <- runtime.createCheckpoint
          _     <- runtime.reset(check.root)
          initialTrie <- runtime.processDeploy(
                          ConstructDeploy.sourceDeploy(trieInitializedRho, 1L, phloLimit = 50000000)
                        )
          _      = assert(!initialTrie.isFailed)
          check2 <- runtime.createCheckpoint
          trieMapHandleR <- runtime.playExploratoryDeploy(
                             getTrieMapHandleRho,
                             check2.root.toByteString
                           )
          _             <- runtime.reset(check2.root)
          trieMapHandle = trieMapHandleR(0)
          maps          <- RhoTrieTraverser.traverseTrie(2, trieMapHandle, runtime)
          goodMap = RhoTrieTraverser.vecParMapToMap(
            maps,
            p => p.exprs(0).getGByteArray,
            p => p.exprs(0).getGInt
          )
          _ = insertKeyValues.map(k => {
            val key = RhoTrieTraverser.keccakKey(k._1).exprs(0).getGByteArray.substring(2, 32)
            assert(goodMap.get(key).get == k._2.toLong)
          })
        } yield ()
    }
    t.runSyncUnsafe()
  }

}
