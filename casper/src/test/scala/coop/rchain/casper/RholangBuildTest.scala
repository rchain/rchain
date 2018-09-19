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
import coop.rchain.rholang.collection.ListOps
import coop.rchain.rholang.interpreter.Runtime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class RholangBuildTest extends FlatSpec with Matchers {

  val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  val bonds                       = HashSetCasperTest.createBonds(validators)
  val genesis                     = HashSetCasperTest.createGenesis(bonds)

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "Our build system" should "allow import of rholang sources into scala code" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.last)
    import node._

    val deploys = Vector(
      "contract @\"double\"(@x, ret) = { ret!(2 * x) }",
      "@(\"ListOps\", \"map\")!([2, 3, 5, 7], \"double\", \"dprimes\")"
    ).zipWithIndex.map { case (d, i) => ProtoUtil.sourceDeploy(d, i.toLong + 1L) }

    val Created(signedBlock) = deploys.traverse(MultiParentCasper[Id].deploy) *> MultiParentCasper[
      Id].createBlock
    val _ = MultiParentCasper[Id].addBlock(signedBlock)

    val storage = HashSetCasperTest.blockTuplespaceContents(signedBlock)

    logEff.warns should be(Nil)
    storage.contains("@{\"dprimes\"}!([4, 6, 10, 14])") should be(true)
  }

}
