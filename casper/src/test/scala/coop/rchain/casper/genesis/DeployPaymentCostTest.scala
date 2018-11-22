package coop.rchain.casper.genesis

import cats.implicits._
import cats.{Id, Monad}
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.DeployPaymentCostTest._
import coop.rchain.casper.genesis.contracts.{Faucet, PreWallet}
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol.{BlockMessage, Deploy, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{genesis => _, _}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.{accounting, InterpolateRholang}
import coop.rchain.rspace.Serialize
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import Matchers._
import scala.collection.immutable.BitSet

class DeployPaymentCostTest extends FlatSpec {

  "DeployPaymentCost" should "estimate cost of deploying payment code" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node.casperEff

    val secKey = Base16.decode(secKeyString)
    val pubKey = Base16.decode(pubKeyString)

    val walletAddress = createWallet(node.runtimeManager)

    val walletUriString = registerWallet(walletAddress, secKey, pubKey, node.runtimeManager)

    val statusChannel = GPrivateBuilder()
    val paymentPar    = makePayment(0L, 10L, secKey, walletUriString, statusChannel)
    val user          = ByteString.copyFrom(pubKey)
    val timestamp     = System.currentTimeMillis()

    val paymentBlock = paymentDeploy(paymentPar, user, timestamp)

    val paymentDeployCost = deployCost(paymentBlock, user, timestamp)

    assertSuccessfulTransfer(node, ProtoUtil.postStateHash(paymentBlock), statusChannel)

    println(s"Cost of deploying payment contract is at minimum: $paymentDeployCost")
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

  // Lookup wallet in the registry and make a transfer
  def paymentDeploy[F[_]: MultiParentCasperImpl: Monad](
      paymentPar: Par,
      user: ByteString,
      timestamp: Long
  ): F[BlockMessage] = {
    val d = Deploy(
      term = Some(paymentPar),
      raw = Some(
        DeployData()
          .withPhloPrice(1L)
          .withPhloLimit(accounting.MAX_VALUE)
          .withUser(user)
          .withTimestamp(timestamp)
          .withTerm(paymentPar.toProtoString)
      )
    )
    deploy[F](d)
  }

  def deploy[F[_]: Monad](
      deploy: Deploy
  )(implicit casperEff: MultiParentCasperImpl[F]): F[BlockMessage] =
    for {
      _              <- casperEff.deploy(deploy)
      b              <- casperEff.createBlock
      Created(block) = b
      blockStatus    <- casperEff.addBlock(block)
      _              = assert(blockStatus == Valid)
    } yield block

  def deployCost(block: BlockMessage, user: ByteString, timestamp: Long): PCost = {
    val costs = for {
      deploy     <- block.body.toList.flatMap(_.deploys)
      deployData <- deploy.deploy.flatMap(_.raw).toList
      if (deployData.user == user && deployData.timestamp == timestamp)
      cost <- deploy.cost
    } yield cost
    assert(costs.size == 1)
    costs.head
  }

  def makePayment(
      n: Long,
      a: Long,
      secKey: Array[Byte],
      walletUri: String,
      statusChannel: Par
  ): Par = {
    val nonce: Par  = GInt(n)
    val amount: Par = GInt(a)

    val transferChannel: Par = GPrivateBuilder()
    val transferTuple: Par   = EList(List(nonce, amount, transferChannel))

    val transferBytes    = bytes(transferTuple)
    val sigTransferBytes = Ed25519.sign(Blake2b256.hash(transferBytes), secKey)

    paymentPar(
      walletUri,
      amount,
      nonce,
      Base16.encode(sigTransferBytes),
      transferChannel,
      statusChannel
    )
  }

  def bytes(par: Par): Array[Byte] = {
    import coop.rchain.models.serialization.implicits._
    Serialize[Par].encode(par).toArray
  }

  // The real payment deploy will be a bit more complicated than this, it will additionally call:
  // * getParams - which will be similar to crypto and stdout which we don't charge for
  // * codePayment - a method that will verify the payment for code deploy
  // but the core of this deployment will be very close to this:
  // * lookup a wallet in the registry
  // * transfer phlos that will be used to pay for the deployment from the wallet
  def paymentPar(
      walletUri: String,
      amount: Par,
      nonce: Par,
      sig: String,
      destination: Par,
      statusChannel: Par
  ): Par = {
    val rho: String =
      s"""
        | new walletChan, lookup(`rho:registry:lookup`) in {
        |   lookup!(`$walletUri`, *walletChan) |
        |   for(@(_, wallet) <- walletChan) {
        |     @wallet!("transfer", "#amount", "#nonce", "$sig", "#destination", "#status")
        |   }
        | }
      """.stripMargin

    InterpolateRholang
      .interpolate(
        rho,
        Map[String, Par](
          "#destination" -> destination,
          "#status"      -> statusChannel,
          "#amount"      -> amount,
          "#nonce"       -> nonce
        )
      )
      .value()
  }

  def createWallet(rm: RuntimeManager)(implicit casper: MultiParentCasperImpl[Id]): Par = {
    // Create new wallet
    val walletRetCh = GPrivateBuilder()
    val newWalletName = deployAndCapture(
      createWalletPar(pubKeyString, walletRetCh),
      walletRetCh,
      rm
    )

    GPrivateBuilder(
      ByteString.copyFrom(
        newWalletName.exprs.head.getEListBody.ps.head.bundles.head.body.ids.head.id.toByteArray
      )
    )
  }

  def deployAndCapture(p: Par, retChannel: Par, rm: RuntimeManager)(
      implicit casper: MultiParentCasperImpl[Id]
  ): Par = {
    val postDeployStateHash = ProtoUtil.postStateHash(deploy[Id](p))
    val data                = rm.getData(postDeployStateHash, retChannel)
    assert(data.size == 1)
    data.head
  }

  def deploy[F[_]: Monad](par: Par)(implicit casperEff: MultiParentCasperImpl[F]): F[BlockMessage] =
    for {
      _              <- casperEff.deploy(ProtoUtil.termDeployNow(par))
      newBlock       <- casperEff.createBlock
      Created(block) = newBlock
      status         <- casperEff.addBlock(block)
      _              = assert(status == Valid)
    } yield block

  def createWalletPar(pubKey: String, retCh: Par): Par = {
    val rho: String =
      s"""new MakeMintCh, mintCh, purseCh, BasicWalletCh, rl(`rho:registry:lookup`) in {
         |  rl!(`rho:id:exunyijimapk7z43g3bbr69awqdz54kyroj9q43jgu3dh567fxsftx`, *MakeMintCh) |
         |  rl!(`rho:id:3yicxut5xtx5tnmnneta7actof4yse3xangw4awzt8c8owqmddgyms`, *BasicWalletCh) |
         |  for(@(_, MakeMint) <- MakeMintCh) {
         |    @MakeMint!(*mintCh) | for(mint <- mintCh) {
         |      mint!("makePurse", 100, *purseCh) | for(@purse <- purseCh) {
         |         for(@(_, basicWallet) <- BasicWalletCh) {
         |          @basicWallet!(purse, "ed25519", "$pubKey", "#retCh")
         |        }
         |      }
         |    }
         |  }
         | }
     """.stripMargin
    InterpolateRholang.interpolate(rho, Map("#retCh" -> retCh)).value
  }

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

  /** Registers wallet in the Registry
    *
    * @param walletAddress unforgeable name of the wallet
    * @return Registry URI of the wallet
    */
  def registerWallet(
      walletAddress: Par,
      secKey: Array[Byte],
      pubKey: Array[Byte],
      rm: RuntimeManager
  )(implicit casper: MultiParentCasperImpl[Id]): String = {
    val registerWalletTuple: Par = ETuple(Seq(GInt(1), walletAddress))
    val registrySig              = Ed25519.sign(registerWalletTuple.toByteArray, secKey)
    assert(Ed25519.verify(registerWalletTuple.toByteArray, registrySig, pubKey))
    val sigHex    = Base16.encode(registrySig)
    val pubKeyHex = Base16.encode(pubKey)

    val walletUriRet: Par = GPrivateBuilder()

    val par = InterpolateRholang
      .interpolate(
        s"""
           |new rs(`rho:registry:insertSigned:ed25519`) in {
           |  rs!("$pubKeyHex".hexToBytes(), (1, "#walletAddress"), "$sigHex".hexToBytes(), "#walletUriRet")
           | }
       """.stripMargin,
        Map("#walletAddress" -> walletAddress, "#walletUriRet" -> walletUriRet)
      )
      .value

    val walletUri = deployAndCapture(par, walletUriRet, rm)

    walletUri.exprs.head.getGUri
  }

  def assertSuccessfulTransfer[F[_]](
      node: HashSetCasperTestNode[F],
      tuplespaceHash: ByteString,
      statusChannel: GPrivate
  ): Unit = {
    val transferStatus = node.runtimeManager.getData(
      tuplespaceHash,
      statusChannel
    )
    assert(transferStatus.size == 1)
    transferStatus.head.exprs.head should be(Expr(GString("Success")))
  }

}
