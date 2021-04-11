package coop.rchain.node.web

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{
  BlockInfo,
  BondInfo,
  DeployInfo,
  JustificationInfo,
  LightBlockInfo,
  MergingDeployStatusProto
}
import coop.rchain.crypto.codec._
import coop.rchain.node.api.WebApi._
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.shared.Log
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.semiauto._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.http4s._
import org.http4s.circe._
import org.scalatest._

class WebApiRoutesTest extends FlatSpec with Matchers {
  val blockHash        = "blockhash"
  val sender           = "sender"
  val seqNum           = 1L
  val sig              = "sig_str"
  val sigAlgorithm     = "secp256k1"
  val shardId          = "root"
  val extraBytes       = ByteString.EMPTY
  val version          = 1L
  val timestamp        = 123L
  val headerExtraBytes = ByteString.EMPTY
  val parentsList      = List("genesis")
  val blockNumber      = 1L
  val preStateHash     = "preHash"
  val postStateHash    = "postHash"
  val bodyExtraBytes   = ByteString.EMPTY
  val bonds            = List[BondInfo](BondInfo("validator_a", 100))
  val blockSize        = "100"
  val deployCount      = 1
  val faultTolerance   = 0.2.toFloat
  val lightBlock = LightBlockInfo(
    blockHash,
    sender,
    seqNum,
    sig,
    sigAlgorithm,
    shardId,
    extraBytes,
    version,
    timestamp,
    headerExtraBytes,
    parentsList,
    blockNumber,
    preStateHash,
    postStateHash,
    bodyExtraBytes,
    bonds,
    blockSize,
    deployCount,
    faultTolerance
  )
  val deployer              = "a"
  val term                  = "@2!(1)"
  val deployTimestamp       = 1000000L
  val deploySig             = "asdf32ascwef"
  val deploySigAlgorithm    = "asd"
  val phloLimit             = 100000L
  val phloPrice             = 1L
  val validAfterBlockNumber = 1L
  val cost                  = 100L
  val errored               = false
  val systemDeployError     = ""
  val deploys = List(
    DeployInfo(
      deployer = deployer,
      term = term,
      timestamp = deployTimestamp,
      sig = deploySig,
      sigAlgorithm = deploySigAlgorithm,
      phloLimit = phloLimit,
      phloPrice = phloPrice,
      validAfterBlockNumber = validAfterBlockNumber,
      cost = cost,
      errored = errored,
      systemDeployError = systemDeployError
    )
  )
  val blockInfo = BlockInfo(
    blockInfo = Some(lightBlock),
    deploys = deploys
  )

  val prepareSeqNumber = 11
  val prepareNames     = List[String]("a", "b", "c")
  val exPRS            = ExprBool(true)
  val deployRet        = "Success!\\nDeployId is: 111111111111"
  val dataAtLength     = 5

  def genWebApi: WebApi[Task] = new WebApi[Task] {
    override def status: Task[WebApi.ApiStatus] =
      Task
        .delay(
          ApiStatus(
            version = 1,
            message = "OK"
          )
        )

    override def prepareDeploy(
        request: Option[WebApi.PrepareRequest]
    ): Task[WebApi.PrepareResponse] =
      Task
        .delay({
          val names     = prepareNames
          val seqNumber = prepareSeqNumber
          WebApi.PrepareResponse(names, seqNumber)
        })

    override def deploy(request: DeployRequest): Task[String] =
      Task.delay(deployRet)

    override def listenForDataAtName(request: WebApi.DataRequest): Task[WebApi.DataResponse] =
      Task
        .delay({
          val exprs = exPRS
          //        List[RhoExpr](ExprBool(true), ExprInt(1), ExprBytes("a"), ExprString("b"), ExprUri("c"))
          val exprsWithBlock = WebApi.RhoExprWithBlock(exprs, lightBlock)
          val data           = List[WebApi.RhoExprWithBlock](exprsWithBlock)
          WebApi.DataResponse(data, dataAtLength)
        })

    override def lastFinalizedBlock: Task[BlockInfo] = Task.delay(blockInfo)

    override def getBlock(hash: String): Task[BlockInfo] = Task.delay(blockInfo)

    override def getBlocks(depth: Int): Task[List[LightBlockInfo]] =
      Task.delay(List(lightBlock))

    override def findDeploy(deployId: String): Task[LightBlockInfo] =
      Task.delay(lightBlock)

    // TODO: https://rchain.atlassian.net/browse/RCHAIN-4018
    override def exploratoryDeploy(
        term: String,
        blockHash: Option[String],
        usePreStateHash: Boolean
    ): Task[ExploratoryDeployResponse] = ???

    override def getBlocksByHeights(
        startBlockNumber: Long,
        endBlockNumber: Long
    ): Task[List[LightBlockInfo]] = ???

    override def isFinalized(hash: String): Task[Boolean] = ???
  }

  def genAdminWebApi: AdminWebApi[Task] = new AdminWebApi[Task] {
    override def propose: Task[String]       = Task.delay("")
    override def proposeResult: Task[String] = Task.delay("")
  }

  implicit val decodeByteString: Decoder[ByteString] = new Decoder[ByteString] {
    override def apply(c: HCursor): Result[ByteString] =
      if (c.value.isString)
        Right(
          Base16
            .decode(c.value.asString.getOrElse(""))
            .map(ByteString.copyFrom)
            .getOrElse(ByteString.EMPTY)
        )
      else Left(DecodingFailure("error", List[CursorOp]()))
  }
  implicit val decodeBondInfo: Decoder[BondInfo] = deriveDecoder[BondInfo]
  implicit val decodeJustificationInfo: Decoder[JustificationInfo] =
    deriveDecoder[JustificationInfo]
  implicit val decodeLightBlockInfo: Decoder[LightBlockInfo] = deriveDecoder[LightBlockInfo]
  implicit val decodeMergingDeployStatusProto: Decoder[MergingDeployStatusProto] =
    deriveDecoder[MergingDeployStatusProto]
  implicit val decodeDeployInfo: Decoder[DeployInfo]             = deriveDecoder[DeployInfo]
  implicit val decodeBlockInfo: Decoder[BlockInfo]               = deriveDecoder[BlockInfo]
  implicit val decodeApiStatus: Decoder[ApiStatus]               = deriveDecoder[ApiStatus]
  implicit val decodeRhoExpr: Decoder[RhoExpr]                   = deriveDecoder[RhoExpr]
  implicit val decodeRhoUnforg: Decoder[RhoUnforg]               = deriveDecoder[RhoUnforg]
  implicit val decodeRhoExprWithBlock: Decoder[RhoExprWithBlock] = deriveDecoder[RhoExprWithBlock]
  implicit val decodeDataResponse: Decoder[DataResponse]         = deriveDecoder[DataResponse]
  implicit val decodePrepareResponse: Decoder[PrepareResponse]   = deriveDecoder[PrepareResponse]
  implicit val encodePrepareRequest: Encoder[PrepareRequest]     = deriveEncoder[PrepareRequest]
  implicit val log                                               = new Log.NOPLog[Task]
  implicit val taskId                                            = natId[Task]

  val api   = genWebApi
  val route = WebApiRoutes.service[Task, Task](api)

  val adminApi   = genAdminWebApi
  val adminRoute = AdminWebApiRoutes.service[Task, Task](adminApi)

  "GET getBlock" should "detailed block info" in {
    val resp     = route.run(Request[Task](method = Method.GET, uri = Uri(path = "block/" + blockHash)))
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res            <- act_resp
      _              = res.status should be(Status.Ok)
      blockInfo      = res.decodeJson[BlockInfo].runSyncUnsafe()
      lightBlockInfo = blockInfo.blockInfo.get
      _              = lightBlockInfo should be(lightBlock)
      deployInfo     = blockInfo.deploys.head
      _              = deployInfo should be(deploys.head)

    } yield ()
  }

  "GET getBlocks" should "return a list of LightBlockInfo" in {
    val resp     = route.run(Request[Task](method = Method.GET, uri = Uri(path = "blocks")))
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res    <- act_resp
      _      = res.status should be(Status.Ok)
      blocks = res.decodeJson[List[LightBlockInfo]].runSyncUnsafe()
      block  <- blocks.headOption
      _      = block should be(lightBlock)
    } yield ()
  }

  "GET getApiStatus" should "return api status" in {
    val resp     = route.run(Request[Task](method = Method.GET, uri = Uri(path = "status")))
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res  <- act_resp
      _    = res.status should be(Status.Ok)
      body = res.decodeJson[ApiStatus].runSyncUnsafe()
      _    = body.message should be("OK")
      _    = body.version should be(1)
    } yield ()
  }

  "GET prepareDeploy" should "return latest block number info" in {
    val resp     = route.run(Request[Task](method = Method.GET, uri = Uri(path = "prepare-deploy")))
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res  <- act_resp
      _    = res.status should be(Status.Ok)
      body = res.decodeJson[PrepareResponse].runSyncUnsafe()
      _    = body.seqNumber should be(prepareSeqNumber)
      _    = body.names should be(prepareNames)
    } yield ()
  }
  "POST prepareDeploy" should "return latest block number info" in {
    import org.http4s.circe._
    val b = """{"deployer":"a", "timestamp":10000000, "nameQty":1}"""
    val resp = route.run(
      Request[Task](method = Method.POST, uri = Uri(path = "prepare-deploy"))
        .withEntity(b)
    )
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res  <- act_resp
      _    = res.status should be(Status.Ok)
      body = res.decodeJson[PrepareResponse].runSyncUnsafe()
      _    = body.seqNumber should be(prepareSeqNumber)
      _    = body.names should be(prepareNames)
    } yield ()
  }
  "POST deploy" should "return deploy message" in {
    val deployReq =
      """{"deployer":"a", "signature":"1sda", "sigAlgorithm":"algorithm",""" +
        """ "data":{"term":"@2!(1)", "timestamp":1000, "phloPrice":1, "phloLimit":10000, "validAfterBlockNumber":1} }"""
    val resp = route.run(
      Request[Task](method = Method.POST, uri = Uri(path = "deploy")).withEntity(deployReq)
    )
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res  <- act_resp
      _    = res.status should be(Status.Ok)
      body = res.decodeJson[String].runSyncUnsafe()
      _    = body should be(deployRet)
    } yield ()
  }

  "POST listenForDataAtName" should "return data at the name" in {
    val requestBody = """{"name": {"UnforgDeploy":{"data":"a"}}, "depth":1}"""
    val resp = route.run(
      Request[Task](method = Method.POST, uri = Uri(path = "data-at-name")).withEntity(requestBody)
    )
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res  <- act_resp
      _    = res.status should be(Status.Ok)
      body = res.decodeJson[DataResponse].runSyncUnsafe()
      _    = body.length should be(dataAtLength)
      _    = body.exprs.head.expr should be(exPRS)
      _    = body.exprs.head.block should be(lightBlock)
    } yield ()
  }

  "GET lastFinalizedBlock" should "return BlockInfo" in {
    val resp =
      route.run(Request[Task](method = Method.GET, uri = Uri(path = "last-finalized-block")))
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res  <- act_resp
      _    = res.status should be(Status.Ok)
      body = res.decodeJson[BlockInfo].runSyncUnsafe()
      _    = body.blockInfo.get should be(lightBlock)
      _    = body.deploys should be(deploys)
    } yield ()
  }

  "GET findDeploy" should "return deployed blockInfo" in {
    val resp =
      route.run(Request[Task](method = Method.GET, uri = Uri(path = "deploy/" + deploySig)))
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res  <- act_resp
      _    = res.status should be(Status.Ok)
      body = res.decodeJson[LightBlockInfo].runSyncUnsafe()
      _    = body should be(lightBlock)
    } yield ()
  }

  "GET unknown uri" should "return 404" in {
    val resp =
      route.run(Request[Task](method = Method.GET, uri = Uri(path = "unknown")))
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res <- act_resp
      _   = res.status should be(Status.NotFound)
    } yield ()
  }

  "POST with incorrect data" should "return 400" in {
    val resp =
      route.run(
        Request[Task](method = Method.POST, uri = Uri(path = "deploy")).withEntity("errorData")
      )
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res <- act_resp
      _   = res.status should be(Status.BadRequest)
    } yield ()
  }

  "POST propose" should "return 200" in {
    val resp =
      adminRoute.run(
        Request[Task](method = Method.POST, uri = Uri(path = "propose"))
      )
    val act_resp = resp.value.runSyncUnsafe()
    for {
      res <- act_resp
      _   = res.status should be(Status.Ok)
    } yield ()
  }
}
