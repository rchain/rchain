package coop.rchain.casper

import cats._
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil

import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519

import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.comm._, CommError._
import coop.rchain.p2p.effects._
import coop.rchain.catscontrib._, ski._, Encryption._

import java.nio.file.Files

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class HashSetCasperTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  val src     = protocolNode("src", 30300)
  val srcKeys = PublicPrivateKeys(Base16.decode("ff00ff00"), Base16.decode("cc00cc00"))
  val nonce   = Base16.decode("00112233")

  val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  val bonds                       = validators.zipWithIndex.map { case (v, i) => v -> (2 * i + 1) }.toMap
  val genesis                     = ProtoUtil.genesisBlock(bonds)
  val storageSize                 = 1024L * 1024
  val storageDirectory            = Files.createTempDirectory("hash-set-casper-test")

  private val appErrId = new ApplicativeError[Id, CommError] {
    def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B]                    = Applicative[Id].ap[A, B](ff)(fa)
    def pure[A](x: A): Id[A]                                          = Applicative[Id].pure[A](x)
    def raiseError[A](e: CommError): Id[A]                            = throw new Exception(CommError.toString)
    def handleErrorWith[A](fa: Id[A])(f: (CommError) => Id[A]): Id[A] = fa
  }

  implicit val logEff            = new LogStub[Id]
  implicit val timeEff           = new LogicalTime[Id]
  implicit val nodeDiscoveryEff  = new NodeDiscoveryStub[Id]()
  implicit val transportLayerEff = new TransportLayerStub[Id](src)
  implicit val encryptionEff     = new EncryptionStub[Id](srcKeys, nonce)
  implicit val keysStoreEff      = new Kvs.InMemoryKvs[Id, PeerNode, Key]
  implicit val errorHandler      = ApplicativeError_.applicativeError[Id, CommError](appErrId)

  override def beforeEach(): Unit = {
    logEff.reset()
    nodeDiscoveryEff.reset()
    transportLayerEff.reset()
    encryptionEff.reset()
    keysStoreEff.keys.map(k => keysStoreEff.delete(k))
  }

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "HashSetCasper" should "accept deploys" in {
    implicit val casperEff =
      MultiParentCasper.hashSetCasper[Id](storageDirectory, storageSize, genesis)

    val deploy = ProtoUtil.basicDeploy(0)
    MultiParentCasper[Id].deploy(deploy)

    logEff.infos.size should be(1)
    logEff.infos.head.contains("CASPER: Received Deploy") should be(true)
  }

  it should "create blocks based on deploys" in {
    implicit val casperEff =
      MultiParentCasper.hashSetCasper[Id](storageDirectory, storageSize, genesis)

    val deploy = ProtoUtil.basicDeploy(0)
    MultiParentCasper[Id].deploy(deploy)

    val Some(block) = MultiParentCasper[Id].createBlock
    val parents     = ProtoUtil.parents(block)
    val deploys     = block.body.get.newCode
    val storage     = blockTuplespaceContents(block)

    parents.size should be(1)
    parents.head should be(genesis.blockHash)
    deploys.size should be(1)
    deploys.head should be(deploy)
    storage.contains("@{0}!(0)") should be(true)
  }

  it should "accept signed blocks" in {
    implicit val casperEff =
      MultiParentCasper.hashSetCasper[Id](storageDirectory, storageSize, genesis)

    val deploy = ProtoUtil.basicDeploy(0)
    MultiParentCasper[Id].deploy(deploy)

    val Some(block) = MultiParentCasper[Id].createBlock
    val signedBlock = ProtoUtil.signBlock(block, validatorKeys.last)

    MultiParentCasper[Id].addBlock(signedBlock)

    val logMessages = List(
      "CASPER: Received Deploy",
      "CASPER: Beginning send of Block #1",
      "CASPER: Added",
      "CASPER: New fork-choice is block"
    )

    logEff.warns.isEmpty should be(true)
    logEff.infos.zip(logMessages).forall { case (a, b) => a.startsWith(b) } should be(true)
    MultiParentCasper[Id].estimator should be(IndexedSeq(signedBlock))
  }

  it should "be able to create a chain of blocks from different deploys" in {
    implicit val casperEff =
      MultiParentCasper.hashSetCasper[Id](storageDirectory, storageSize, genesis)

    val deploys = Vector(
      "contract @\"add\"(@x, @y, ret) = { ret!(x + y) }",
      "new unforgable in { @\"add\"!(5, 7, *unforgable) }"
    ).map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))

    val Some(block1) = MultiParentCasper[Id].deploy(deploys.head) *> MultiParentCasper[Id].createBlock
    val signedBlock1 = ProtoUtil.signBlock(block1, validatorKeys.last)
    MultiParentCasper[Id].addBlock(signedBlock1)

    val Some(block2) = MultiParentCasper[Id].deploy(deploys(1)) *> MultiParentCasper[Id].createBlock
    val signedBlock2 = ProtoUtil.signBlock(block2, validatorKeys.head)
    MultiParentCasper[Id].addBlock(signedBlock2)
    val storage = blockTuplespaceContents(signedBlock2)

    logEff.warns should be(Nil)
    ProtoUtil.parents(signedBlock2) should be(Seq(signedBlock1.blockHash))
    MultiParentCasper[Id].estimator should be(IndexedSeq(signedBlock2))
    storage.contains("!(12)") should be(true)
  }

  it should "reject unsigned blocks" in {
    implicit val casperEff =
      MultiParentCasper.hashSetCasper[Id](storageDirectory, storageSize, genesis)

    val Some(block) = MultiParentCasper[Id].deploy(ProtoUtil.basicDeploy(0)) *> MultiParentCasper[
      Id].createBlock

    MultiParentCasper[Id].addBlock(block)

    logEff.warns.head.contains("CASPER: Ignoring block") should be(true)
  }

  private def blockTuplespaceContents(block: BlockMessage)(
      implicit casper: MultiParentCasper[Id]): String = {
    val tsHash           = block.body.get.postState.get.tuplespace
    val Some(checkpoint) = MultiParentCasper[Id].tsCheckpoint(tsHash)
    val storage = {
      val ts     = checkpoint.toTuplespace
      val result = ts.storageRepr
      ts.delete()
      result
    }
    storage
  }

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def protocolNode(name: String, port: Int): ProtocolNode =
    ProtocolNode(peerNode(name, port))

}
