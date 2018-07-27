package coop.rchain.casper.util.comm

import cats.Id
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.HashSetCasperTest
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.p2p.EffectsTestInstances._

import org.scalatest.{FlatSpec, Matchers}

class ApproveBlockProtocolTest extends FlatSpec with Matchers {
  "ApproveBlockProtocol" should "add valid signatures it recieves to its state" in {
    val abp = ApproveBlockProtocolTest.createIdProtocol(10, 100L)
    val a   = ApproveBlockProtocolTest.approval(abp.candidate)

    abp.currentSigs.size should be(0)
    abp.addApproval(a)
    abp.currentSigs.size should be(1)
  }

  it should "not change the number of signatures in its state if the same one is given multiple times" in {
    val abp = ApproveBlockProtocolTest.createIdProtocol(10, 100L)
    val a   = ApproveBlockProtocolTest.approval(abp.candidate)

    abp.currentSigs.size should be(0)
    abp.addApproval(a)
    abp.currentSigs.size should be(1)
    abp.addApproval(a)
    abp.currentSigs.size should be(1)
  }

  it should "not add invalid signatures it recieves to its state" in {
    val abp = ApproveBlockProtocolTest.createIdProtocol(10, 100L)
    val a   = ApproveBlockProtocolTest.invalidApproval(abp.candidate)

    abp.currentSigs.size should be(0)
    abp.addApproval(a)
    abp.currentSigs.size should be(0)
  }

  it should "create an approved block if at least the correct number of signatures is collected after the duration has elapsed" in {
    val n: Int        = 10
    val d: Long       = 30L
    implicit val abp  = ApproveBlockProtocolTest.createIdProtocol(n, d)
    implicit val time = new LogicalTime[Id]()
    val c             = abp.candidate
    (1 to n).foreach(_ => abp.addApproval(ApproveBlockProtocolTest.approval(c)))

    ApproveBlockProtocol.run[Id](0L)

    abp.approvedBlock.nonEmpty should be(true)
    time.clock should be(d + 1)
  }

  it should "continue collecting signatures if not enough are collected after the duration has elapsed" in {
    val n: Int        = 10
    val d: Long       = 30L
    implicit val abp  = ApproveBlockProtocolTest.createIdProtocol(n, d)
    implicit val time = new LogicalTime[Id]()
    val c             = abp.candidate

    (1 to (n / 2)).foreach(_ => abp.addApproval(ApproveBlockProtocolTest.approval(c)))
    time.clock = d + 1
    abp.approvedBlock.nonEmpty should be(false)

    (1 to (n / 2 + 1)).foreach(_ => abp.addApproval(ApproveBlockProtocolTest.approval(c)))
    ApproveBlockProtocol.run[Id](0L)
    abp.approvedBlock.nonEmpty should be(true)
  }

  it should "skip the duration and create and approved block immediately if the required signatures is zero" in {
    val d: Long       = 30L
    implicit val abp  = ApproveBlockProtocolTest.createIdProtocol(0, d)
    implicit val time = new LogicalTime[Id]()

    ApproveBlockProtocol.run[Id](0L)

    abp.approvedBlock.nonEmpty should be(true)
    time.clock should be(1L)
  }
}

object ApproveBlockProtocolTest {
  def approval(c: ApprovedBlockCandidate): BlockApproval = {
    val (sk, pk) = Ed25519.newKeyPair
    val sigData  = Blake2b256.hash(c.toByteArray)
    val sig      = Ed25519.sign(sigData, sk)
    BlockApproval(Some(c),
                  Some(Signature(ByteString.copyFrom(pk), "ed25519", ByteString.copyFrom(sig))))
  }

  def invalidApproval(c: ApprovedBlockCandidate): BlockApproval = {
    val (sk, pk) = Ed25519.newKeyPair
    val sigData  = Blake2b256.hash(c.toByteArray ++ "wrong data".toArray.map(_.toByte))
    val sig      = Ed25519.sign(sigData, sk)
    BlockApproval(Some(c),
                  Some(Signature(ByteString.copyFrom(pk), "ed25519", ByteString.copyFrom(sig))))
  }

  def createIdProtocol(requiredSigs: Int, duration: Long): ApproveBlockProtocol[Id] = {
    import monix.execution.Scheduler.Implicits.global

    val (sk, pk) = Ed25519.newKeyPair
    val genesis  = HashSetCasperTest.createGenesis(Seq(pk))
    val node     = HashSetCasperTestNode.standalone(genesis, sk)
    import node._

    ApproveBlockProtocol.create[Id](genesis, requiredSigs, duration)
  }
}
