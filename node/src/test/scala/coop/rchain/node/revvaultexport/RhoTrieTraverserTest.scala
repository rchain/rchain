package coop.rchain.node.revvaultexport

import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO}
import coop.rchain.casper.genesis.contracts.{Registry, StandardDeploys}
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.rholang.RhoType.RhoName
import coop.rchain.models.syntax._
import coop.rchain.shared.Log
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.compat.immutable.LazyList
import scala.util.Random

class RhoTrieTraverserTest extends AnyFlatSpec {
  private val SHARD_ID = "root-shard"
  private val registry = Registry(GenesisBuilder.defaultSystemContractPubKey)

  "traverse the TreeHashMap" should "work" in {
    val total     = 100
    val trieDepth = 2
    val insertKeyValues = (0 to total).map(
      i => (Random.alphanumeric.take(10).foldLeft("")(_ + _), Random.nextInt(1000000), i)
    )
    val insertRho = insertKeyValues.foldLeft("") {
      case (acc, (key, value, index)) =>
        if (index != total)
          acc + s"""new a in {@TreeHashMap!("set", treeMap, "${key}", ${value}, *a)}|\n"""
        else acc + s"""new a in {@TreeHashMap!("set", treeMap, "${key}", ${value}, *a)}\n"""
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
        |  for (@(_, TreeHashMap) <- TreeHashMapCh){
        |    @TreeHashMap!("init", ${trieDepth}, *vaultMapStore) |
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

    implicit val concurrent                  = Async[IO]
    implicit val metricsEff: Metrics[Effect] = new Metrics.MetricsNOP[IO]
    implicit val noopSpan: Span[Effect]      = NoopSpan[IO]()
    implicit val logger: Log[Effect]         = Log.log[IO]
    val t = rhoRuntimeEff[Effect](false).use {
      case (runtime, _, _) =>
        for {
          hash1 <- runtime.emptyStateHash
          _     <- runtime.reset(hash1.toBlake2b256Hash)
          rand  = Blake2b512Random.defaultRandom
          storeToken = {
            val r      = rand.copy()
            val target = LazyList.continually(r.next()).drop(9).head
            RhoName(target)
          }
          rd <- runtime.processDeploy(
                 StandardDeploys.registryGenerator(registry, SHARD_ID),
                 rand
               )
          check <- runtime.createCheckpoint
          _     <- runtime.reset(check.root)
          initialTrieRes <- runtime.processDeploy(
                             ConstructDeploy
                               .sourceDeploy(
                                 trieInitializedRho,
                                 1L,
                                 phloLimit = 50000000
                               ),
                             Blake2b512Random.defaultRandom
                           )
          (initialTrie, _) = initialTrieRes
          _                = assert(!initialTrie.isFailed)
          check2           <- runtime.createCheckpoint
          trieMapHandleR <- runtime.playExploratoryDeploy(
                             getTrieMapHandleRho,
                             check2.root.toByteString
                           )
          _             <- runtime.reset(check2.root)
          trieMapHandle = trieMapHandleR.head
          maps          <- RhoTrieTraverser.traverseTrie(trieDepth, trieMapHandle, storeToken, runtime)
          goodMap = RhoTrieTraverser.vecParMapToMap(
            maps,
            p => p.exprs.head.getGByteArray,
            p => p.exprs.head.getGInt
          )
          _ = insertKeyValues.map(k => {
            val key =
              RhoTrieTraverser.keccakKey(k._1).exprs.head.getGByteArray.substring(trieDepth, 32)
            assert(goodMap(key) == k._2.toLong)
          })
        } yield ()
    }
    t.unsafeRunSync()
  }

}
