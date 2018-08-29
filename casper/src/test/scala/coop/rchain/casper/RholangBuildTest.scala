package coop.rchain.casper

import java.nio.file.Files

import cats.Id
import cats.implicits._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, ProofOfStakeValidator}
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol.{Deploy, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.collection.LinkedList
import coop.rchain.rholang.interpreter.Runtime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class RholangBuildTest extends FlatSpec with Matchers {

  val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  val bonds                       = validators.zipWithIndex.map { case (v, i) => v -> (2 * i + 1) }.toMap
  val initial                     = Genesis.withoutContracts(bonds, 0L, 0L, "rchain")
  val storageDirectory            = Files.createTempDirectory(s"rholang-build-test-genesis")
  val storageSize: Long           = 1024L * 1024
  val activeRuntime               = Runtime.create(storageDirectory, storageSize)
  val runtimeManager              = RuntimeManager.fromRuntime(activeRuntime)
  val emptyStateHash              = runtimeManager.emptyStateHash
  val proofOfStakeValidators      = bonds.map(bond => ProofOfStakeValidator(bond._1, bond._2)).toSeq
  val proofOfStakeDeploy =
    ProtoUtil.termDeploy(new ProofOfStake(proofOfStakeValidators).term, System.currentTimeMillis())
  val genesis =
    Genesis.withContracts(List[Deploy](proofOfStakeDeploy), initial, emptyStateHash, runtimeManager)
  activeRuntime.close()

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "Our build system" should "allow import of rholang sources into scala code" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.last)
    import node._

    val llDeploy  = ProtoUtil.sourceDeploy(LinkedList.code, System.currentTimeMillis())
    val startTime = System.currentTimeMillis()
    val deploys = Vector(
      "@[\"LinkedList\", \"fromList\"]!([2, 3, 5, 7], \"primes\")",
      "contract @\"double\"(@x, ret) = { ret!(2 * x) }",
      "for(@primes <- @\"primes\"){ @\"primes\"!(primes) | @[\"LinkedList\", \"map\"]!(primes, \"double\", \"dprimes\") }"
    ).zipWithIndex.map(d => ProtoUtil.sourceDeploy(d._1, d._2))

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
