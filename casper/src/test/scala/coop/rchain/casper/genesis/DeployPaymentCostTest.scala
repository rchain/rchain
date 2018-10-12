package coop.rchain.casper.genesis

import cats.implicits._
import cats.{Apply, Id}
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.DeployPaymentCostTest._
import coop.rchain.casper.genesis.contracts.{Faucet, PreWallet}
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol.{BlockMessage, Deploy, DeployData, PhloPrice}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{Created, HashSetCasperTest, MultiParentCasperImpl, Valid}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.{accounting, Interpreter}
import coop.rchain.rspace.Serialize
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FlatSpec

import scala.collection.immutable.BitSet

class DeployPaymentCostTest extends FlatSpec {

  "DeployPaymentCost" should "estimate cost of deploying payment code" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node.casperEff

    val secKey = Base16.decode(secKeyString)
    val pubKey = Base16.decode(pubKeyString)

    // Create new wallet
    val walletRetCh = "walletRet"
    val newWalletName = deployAndCapture(
      createWalletPar(pubKeyString, walletRetCh),
      GString(walletRetCh),
      node.runtimeManager
    )
    val walletAddress =
      GPrivateBuilder(
        ByteString.copyFrom(newWalletName.exprs.head.getEListBody.ps.head.ids.head.id.toByteArray)
      )

    // Register wallet in the Registry
    val registerWalletTuple: Par = ETuple(Seq(GInt(1), walletAddress))
    val registrySig              = Ed25519.sign(registerWalletTuple.toByteArray, secKey)
    Ed25519.verify(registerWalletTuple.toByteArray, registrySig, pubKey)

    val walletUriRet: Par = GPrivateBuilder()

    val registerWalletPar = registerSigned(
      registerWalletTuple,
      ByteString.copyFrom(pubKey),
      ByteString.copyFrom(registrySig),
      walletUriRet
    )

    val walletUri = deployAndCapture(registerWalletPar, walletUriRet, node.runtimeManager)

    val walletUriString = walletUri.exprs.head.getGUri

    // Prepare wallet transfer
    val nonce  = 0
    val amount = 10
    //TODO: use unforgeable names

    val transferChannel  = "myWithdrawal"
    val statusChannel    = "withdrawalStatus"
    val transfer         = parse(s"""[$nonce, $amount, "$transferChannel"]""")
    val transferBytes    = bytes(transfer)
    val sigTransferBytes = Ed25519.sign(Blake2b256.hash(transferBytes), secKey)

    val user      = ByteString.copyFrom(pubKey)
    val timestamp = System.currentTimeMillis()

    // Lookup wallet in the registry and make a transfer
    val paymentStr =
      paymentRho(
        walletUriString,
        amount,
        nonce,
        Base16.encode(sigTransferBytes),
        transferChannel,
        statusChannel
      )

    val paymentDeployData =
      DeployData()
        .withPhloPrice(PhloPrice(1))
        .withPhloLimit(accounting.MAX_VALUE)
        .withUser(user)
        .withTimestamp(timestamp)
        .withTerm(paymentStr)

    val paymentBlock = deploy[Id](paymentDeployData)
    val paymentDeployCost = for {
      deploy     <- paymentBlock.body.toList.flatMap(_.deploys)
      deployData <- deploy.deploy.flatMap(_.raw).toList
      if (deployData.user == user && deployData.timestamp == timestamp)
      cost <- deploy.cost
    } yield cost

    assert(paymentDeployCost.size == 1)
    // assert that transfer was successful
    val captureTransferStatusChannel = GPrivateBuilder()
    val transferStatus = node.runtimeManager.captureResults(
      paymentBlock.getBody.getPostState.tuplespace,
      captureSingleResult(GString(statusChannel), captureTransferStatusChannel),
      captureTransferStatusChannel
    )
    assert(transferStatus.size == 1)
    assert(transferStatus.head.exprs.head == Expr(GString("Success")))

    println(s"Cost of deploying payment contract is at minimum: ${paymentDeployCost.head.cost}")
  }

}

object DeployPaymentCostTest {
  val pubKeyString: String = "11afb9a5fa2b3e194b701987b3531a93dbdf790dac26f8a2502cfa5d529f6b4d"
  val secKeyString: String = "d039d5c634ad95d968fc18368d81b97aaecd32fc7cf6eec07a97c5ac9f9fcb5b"

  import HashSetCasperTest._

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val (_, ethPubKeys)             = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val ethAddresses =
    ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
  private val wallets     = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
  private val bonds       = createBonds(validators)
  private val minimumBond = 100L
  private val genesis =
    buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)

  def bytes(par: Par): Array[Byte] = {
    import coop.rchain.models.serialization.implicits._
    Serialize[Par].encode(par).toArray
  }

  def deploy[F[_]: Apply](
      dd: DeployData
  )(implicit casperEff: MultiParentCasperImpl[F]): BlockMessage = {
    val Created(block) = casperEff.deploy(dd) *> casperEff.createBlock
    val blockStatus    = casperEff.addBlock(block)
    assert(blockStatus == Valid)
    block
  }

  def deploy[F[_]: Apply](
      deploy: Deploy
  )(implicit casperEff: MultiParentCasperImpl[F]): BlockMessage = {
    val Created(block) = casperEff.deploy(deploy) *> casperEff.createBlock
    val blockStatus    = casperEff.addBlock(block)
    assert(blockStatus == Valid)
    block
  }

  def deploy[F[_]: Apply](par: Par)(implicit casperEff: MultiParentCasperImpl[F]): BlockMessage = {
    val parDeploy      = ProtoUtil.termDeployNow(par)
    val Created(block) = casperEff.deploy(parDeploy) *> casperEff.createBlock
    val blockStatus    = casperEff.addBlock(block)
    assert(blockStatus == Valid)
    block
  }

  def deployAndCapture(p: Par, retChannel: Par, rm: RuntimeManager)(
      implicit casper: MultiParentCasperImpl[Id]
  ): Par = {
    val postDeployStateHash = deploy[Id](p).getBody.getPostState.tuplespace
    val captureChan         = GPrivateBuilder()
    val res = rm.captureResults(
      postDeployStateHash,
      captureSingleResult(retChannel, captureChan),
      captureChan
    )
    assert(res.size == 1)
    res.head
  }

  def registerSigned(tuple: Par, pubKey: ByteString, sig: ByteString, retChannel: Par): Par =
    New(
      uri = Seq("rho:registry:insertSigned:ed25519"),
      bindCount = 1,
      locallyFree = AlwaysEqual(BitSet(0)),
      p = Send(
        chan = Par().withExprs(Seq(Expr().withEVarBody(EVar(BoundVar(0))))),
        data = Seq(
          GByteArray(pubKey),
          tuple,
          GByteArray(sig),
          retChannel
        )
      )
    )

  def captureSingleResult(source: Par, forwardChan: Par): Par = Receive(
    binds = Seq(
      ReceiveBind(
        patterns = Seq(EVar(FreeVar(0))),
        source = source,
        freeCount = 1
      )
    ),
    body = Send(forwardChan, Seq(EVar(BoundVar(0))), false, AlwaysEqual(BitSet(0))),
    persistent = false,
    bindCount = 1,
    locallyFree = AlwaysEqual(BitSet(0)),
    connectiveUsed = false
  )

  def createWalletPar(secKey: String, retCh: String): Par =
    parse(createWalletRho(secKey, retCh))

  def parse(rho: String): Par =
    Interpreter.buildNormalizedTerm(rho).value()

  //TODO: use unforgeable names
  def createWalletRho(pubKey: String, retCh: String): String =
    s"""
        @"MakeMint"!("mint") |
        for(@mint <- @"mint") {
          @(mint, "makePurse")!(100, "purse") |
          for(@purse <- @"purse") {
            @"BasicWallet"!(purse, "ed25519", "$pubKey", "$retCh")
          }
        }
      """.stripMargin

  // TODO: Update once the actual short leash deployment finalize
  // TODO: Use unforgeable names
  // The real payment deploy will be a bit more complicated than this, it will additionally call:
  // * getParams - which will be similar to crypto and stdout which we don't charge for
  // * codePayment - a method that will verify the payment for code deploy
  // but the core of this deployment will be very close to this:
  // * lookup a wallet in the registry
  // * transfer phlos that will be used to pay for the deployment from the wallet
  def paymentRho(
      walletUri: String,
      amount: Int,
      nonce: Int,
      sig: String,
      destination: String,
      statusChannel: String
  ): String =
    s"""
    new walletChan, lookup(`rho:registry:lookup`) in {
      lookup!(`$walletUri`, *walletChan) |
        for (@(_, wallet) <- walletChan) {
          @(wallet, "transfer")!($amount, $nonce, "$sig", "$destination", "$statusChannel")
        }
      }"""
}
