package coop.rchain.casper

import cats.Id
import cats.implicits._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.casper.genesis.contracts.LinkedList
import coop.rchain.crypto.signatures.Ed25519
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class RholangBuildTest extends FlatSpec with Matchers {

  val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  val bonds                       = validators.zipWithIndex.map { case (v, i) => v -> (2 * i + 1) }.toMap
  val genesis                     = ProtoUtil.genesisBlock(bonds)

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "Our build system" should "allow import of rholang sources into scala code" in {
    val node = HashSetCasperTestNode.standalone(genesis)
    import node._

    val llDeploy = ProtoUtil.termDeploy(LinkedList.term)
    val deploys = Vector(
      "@[\"LinkedList\", \"fromList\"]!([2, 3, 5, 7], \"primes\")",
      "contract @\"double\"(@x, ret) = { ret!(2 * x) }",
      "for(@primes <- @\"primes\"){ @\"primes\"!(primes) | @[\"LinkedList\", \"map\"]!(primes, \"double\", \"dprimes\") }"
    ).map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))

    val Some(block) = MultiParentCasper[Id].deploy(llDeploy) *> deploys.traverse(
      MultiParentCasper[Id].deploy) *> MultiParentCasper[Id].createBlock
    val signedBlock = ProtoUtil.signBlock(block, validatorKeys.last)
    MultiParentCasper[Id].addBlock(signedBlock)

    val storage = HashSetCasperTest.blockTuplespaceContents(signedBlock)

    logEff.warns should be(Nil)
    storage.contains("@{\"primes\"}!([2, [3, [5, [7, []]]]])") should be(true)
    storage.contains("@{\"dprimes\"}!([4, [6, [10, [14, []]]]])") should be(true)
  }

}
