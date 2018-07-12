package coop.rchain.casper

import java.nio.file.Files

import cats.Id
import cats.implicits._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol.{BlockMessage, Deploy}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.collection.LinkedList
import coop.rchain.rholang.interpreter.Runtime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.SyncVar

class RholangBuildTest extends FlatSpec with Matchers {

  val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  val bonds                       = validators.zipWithIndex.map { case (v, i) => v -> (2 * i + 1) }.toMap
  val initial                     = Genesis.withoutContracts(bonds = bonds, version = 0L, timestamp = 0L)
  val storageDirectory            = Files.createTempDirectory(s"rholang-build-test-genesis")
  val storageSize: Long           = 1024L * 1024
  val activeRuntime               = Runtime.create(storageDirectory, storageSize)
  val runtime                     = new SyncVar[Runtime]()
  runtime.put(activeRuntime)
  val (initStateHash, runtimeManager) = RuntimeManager.fromRuntime(runtime)
  val genesis =
    Genesis.withContracts(List.empty[Deploy], initial, Nil, initStateHash, runtimeManager)
  activeRuntime.close()

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "Our build system" should "allow import of rholang sources into scala code" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.last)
    import node._

    val llDeploy = ProtoUtil.termDeploy(LinkedList.term)
    val deploys = Vector(
      "@[\"LinkedList\", \"fromList\"]!([2, 3, 5, 7], \"primes\")",
      "contract @\"double\"(@x, ret) = { ret!(2 * x) }",
      "for(@primes <- @\"primes\"){ @\"primes\"!(primes) | @[\"LinkedList\", \"map\"]!(primes, \"double\", \"dprimes\") }"
    ).map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))

    val Some(signedBlock) = MultiParentCasper[Id].deploy(llDeploy) *>
      deploys.traverse(MultiParentCasper[Id].deploy) *>
      MultiParentCasper[Id].createBlock
    MultiParentCasper[Id].addBlock(signedBlock)

    val storage = HashSetCasperTest.blockTuplespaceContents(signedBlock)

    logEff.warns should be(Nil)
    storage.contains("@{\"primes\"}!([2, [3, [5, [7, []]]]])") should be(true)
    storage.contains("@{\"dprimes\"}!([4, [6, [10, [14, []]]]])") should be(true)
  }

}
