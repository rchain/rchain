package coop.rchain.casper.api

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.api.BlockAPI_v2
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.DeployError._
import coop.rchain.casper._
import coop.rchain.casper.api.BlockAPI.{
  BlockRetrievalError,
  NoBlockMessageError,
  ValidatorReadOnlyError
}
import coop.rchain.casper.blocks.proposer.ProposeResult._
import coop.rchain.casper.blocks.proposer._
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol._
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.casper.syntax._
import coop.rchain.casper.util._
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager, Tools}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.graphz._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.rholang.sorter.Sortable._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockMetadata, NormalizerEnv, Par}
import coop.rchain.rspace.hashing.StableHashProvider
import coop.rchain.rspace.trace._
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream

import scala.collection.immutable.SortedMap

class BlockAPIImpl[F[_]: Concurrent: Span: DeployStorage: BlockStore: BlockDagStorage: MultiParentCasper: Log](
    runtimeManager: RuntimeManager[F],
    dagStorage: BlockDagStorage[F],
    validatorOpt: Option[ValidatorIdentity]
) extends BlockAPI_v2[F] {

  val blockAPIMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "block-api")
  val deploySource: Metrics.Source          = Metrics.Source(blockAPIMetricsSource, "deploy")
  val getBlockSource: Metrics.Source        = Metrics.Source(blockAPIMetricsSource, "get-block")

  override def deploy(
      d: Signed[DeployData],
      triggerPropose: Option[ProposeFunction[F]],
      minPhloPrice: Long,
      isNodeReadOnly: Boolean,
      shardId: String
  ): F[ApiErr[String]] = Span[F].trace(deploySource) {

    def casperDeploy: F[ApiErr[String]] =
      for {
        r <- makeDeploy(d).map(
              _.bimap(
                err => err.show,
                res => s"Success!\nDeployId is: ${PrettyPrinter.buildStringNoLimit(res)}"
              )
            )
        // call a propose if proposer defined
        _ <- triggerPropose.traverse(_(MultiParentCasper[F], true))
      } yield r

    // Check if node is read-only
    val readOnlyError = new RuntimeException(
      "Deploy was rejected because node is running in read-only mode."
    ).raiseError[F, ApiErr[String]]
    val readOnlyCheck = readOnlyError.whenA(isNodeReadOnly)

    // Check if deploy's shardId equals to node shardId
    val shardIdError = new RuntimeException(
      s"Deploy shardId '${d.data.shardId}' is not as expected network shard '$shardId'."
    ).raiseError[F, ApiErr[String]]
    val shardIdCheck = shardIdError.whenA(d.data.shardId != shardId)

    // Check if deploy is signed with system keys
    val isForbiddenKey = StandardDeploys.systemPublicKeys.contains(d.pk)
    val forbiddenKeyError = new RuntimeException(
      s"Deploy refused because it's signed with forbidden private key."
    ).raiseError[F, ApiErr[String]]
    val forbiddenKeyCheck = forbiddenKeyError.whenA(isForbiddenKey)

    // Check if deploy has minimum phlo price
    val minPriceError = new RuntimeException(
      s"Phlo price ${d.data.phloPrice} is less than minimum price $minPhloPrice."
    ).raiseError[F, ApiErr[String]]
    val minPhloPriceCheck = minPriceError.whenA(d.data.phloPrice < minPhloPrice)

    readOnlyCheck >> shardIdCheck >> forbiddenKeyCheck >> minPhloPriceCheck >> casperDeploy
  }

  private def makeDeploy(d: Signed[DeployData]): F[Either[DeployError, DeployId]] = {
    import coop.rchain.models.rholang.implicits._

    def addDeploy(deploy: Signed[DeployData]): F[DeployId] =
      for {
        _ <- DeployStorage[F].add(List(deploy))
        _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
      } yield deploy.sig

    InterpreterUtil
      .mkTerm(d.data.term, NormalizerEnv(d))
      .bitraverse(
        err => DeployError.parsingError(s"Error in parsing term: \n$err").pure[F],
        _ => addDeploy(d)
      )
  }

  override def createBlock(
      triggerProposeF: ProposeFunction[F],
      isAsync: Boolean = false
  ): F[ApiErr[String]] = {
    def logDebug(err: String)  = Log[F].debug(err) >> err.asLeft[String].pure[F]
    def logSucess(msg: String) = Log[F].info(msg) >> msg.asRight[Error].pure[F]
    for {
      // Trigger propose
      proposerResult <- triggerProposeF(MultiParentCasper[F], isAsync)
      r <- proposerResult match {
            case ProposerEmpty =>
              logDebug(s"Failure: another propose is in progress")
            case ProposerFailure(status, seqNumber) =>
              logDebug(s"Failure: $status (seqNum $seqNumber)")
            case ProposerStarted(seqNumber) =>
              logSucess(s"Propose started (seqNum $seqNumber)")
            case ProposerSuccess(_, block) =>
              // TODO: [WARNING] Format of this message is hardcoded in pyrchain when checking response result
              //  Fix to use structured result with transport errors/codes.
              // https://github.com/rchain/pyrchain/blob/a2959c75bf/rchain/client.py#L42
              val blockHashHex = block.blockHash.toHexString
              logSucess(s"Success! Block $blockHashHex created and added.")
          }
    } yield r
  }

  // Get result of the propose
  override def getProposeResult(proposerState: Ref[F, ProposerState[F]]): F[ApiErr[String]] =
    for {
      pr <- proposerState.get.map(_.currProposeResult)
      r <- pr match {
            // return latest propose result
            case None =>
              for {
                result <- proposerState.get.map(
                           _.latestProposeResult.getOrElse(ProposeResult.notEnoughBlocks, None)
                         )
                msg = result._2 match {
                  case Some(block) =>
                    s"Success! Block ${block.blockHash.toHexString} created and added."
                      .asRight[Error]
                  case None => s"${result._1.proposeStatus.show}".asLeft[String]
                }
              } yield msg
            // wait for current propose to finish and return result
            case Some(resultDef) =>
              for {
                // this will hang API call until propose is complete, and then return result
                // TODO cancel this get when connection drops
                result <- resultDef.get
                msg = result._2 match {
                  case Some(block) =>
                    s"Success! Block ${block.blockHash.toHexString} created and added."
                      .asRight[Error]
                  case None => s"${result._1.proposeStatus.show}".asLeft[String]
                }
              } yield msg
          }
    } yield r

  /**
    * Performs transformation on a stream of blocks (from highest height, same heights loaded as batch)
    *
    * @param heightMap height map to iterate block hashes
    * @param transform function to run for each block
    * @return stream of transform results
    */
  private def getFromBlocks[A](
      heightMap: SortedMap[Long, Set[BlockHash]]
  )(transform: BlockMessage => F[A]): Stream[F, List[A]] = {
    // TODO: check if conversion of SortedMap#toIndexedSeq is performant enough
    val reverseHeightMap = heightMap.toIndexedSeq.reverse
    val iterBlockHashes  = reverseHeightMap.iterator.map(_._2.toList)
    Stream
      .fromIterator(iterBlockHashes)
      .evalMap(_.traverse(BlockStore[F].getUnsafe))
      .evalMap(_.traverse(transform))
  }

  override def getListeningNameDataResponse(
      depth: Int,
      listeningName: Par,
      maxBlocksLimit: Int
  ): F[ApiErr[(Seq[DataWithBlockInfo], Int)]] = {
    val response: F[Either[Error, (Seq[DataWithBlockInfo], Int)]] = for {
      dag                 <- dagStorage.getRepresentation
      heightMap           = dag.heightMap
      depthWithLimit      = Math.min(depth, maxBlocksLimit).toLong
      sortedListeningName <- parSortable.sortMatch[F](listeningName).map(_.term)
      blockDataStream = getFromBlocks(heightMap) { block =>
        getDataWithBlockInfo(sortedListeningName, block)
      }
      // For compatibility with v0.12.x depth must include all blocks with the same height
      //  e.g. depth=1 should always return latest block created by the node
      blocksWithActiveName <- blockDataStream
                               .map(_.flatten)
                               .take(depthWithLimit)
                               .compile
                               .toList
                               .map(_.flatten)
    } yield (blocksWithActiveName, blocksWithActiveName.length).asRight[String]
    // Check depth limit
    if (depth > maxBlocksLimit)
      s"Your request on getListeningName depth $depth exceed the max limit $maxBlocksLimit"
        .asLeft[(Seq[DataWithBlockInfo], Int)]
        .pure[F]
    else response
  }

  override def getListeningNameContinuationResponse(
      depth: Int,
      listeningNames: Seq[Par],
      maxBlocksLimit: Int
  ): F[ApiErr[(Seq[ContinuationsWithBlockInfo], Int)]] = {
    val response: F[Either[Error, (Seq[ContinuationsWithBlockInfo], Int)]] = for {
      dag            <- dagStorage.getRepresentation
      heightMap      = dag.heightMap
      depthWithLimit = Math.min(depth, maxBlocksLimit).toLong
      sortedListeningNames <- listeningNames.toList
                               .traverse(parSortable.sortMatch[F](_).map(_.term))
      blockDataStream = getFromBlocks(heightMap) { block =>
        getContinuationsWithBlockInfo(sortedListeningNames, block)
      }
      // For compatibility with v0.12.x depth must include all blocks with the same height
      //  e.g. depth=1 should always return latest block created by the node
      blocksWithActiveName <- blockDataStream
                               .map(_.flatten)
                               .take(depthWithLimit)
                               .compile
                               .toList
                               .map(_.flatten)
    } yield (blocksWithActiveName, blocksWithActiveName.length).asRight[String]
    // Check depth limit
    if (depth > maxBlocksLimit)
      s"Your request on getListeningNameContinuation depth $depth exceed the max limit $maxBlocksLimit"
        .asLeft[(Seq[ContinuationsWithBlockInfo], Int)]
        .pure[F]
    else response
  }

  private def getDataWithBlockInfo(
      sortedListeningName: Par,
      block: BlockMessage
  ): F[Option[DataWithBlockInfo]] =
    // TODO: For Produce it doesn't make sense to have multiple names
    if (isListeningNameReduced(block, Seq(sortedListeningName))) {
      val stateHash = ProtoUtil.postStateHash(block)
      for {
        data      <- runtimeManager.getData(stateHash)(sortedListeningName)
        blockInfo <- getLightBlockInfo(block)
      } yield Option[DataWithBlockInfo](DataWithBlockInfo(data, blockInfo))
    } else {
      none[DataWithBlockInfo].pure[F]
    }

  private def getContinuationsWithBlockInfo(
      sortedListeningNames: Seq[Par],
      block: BlockMessage
  ): F[Option[ContinuationsWithBlockInfo]] =
    if (isListeningNameReduced(block, sortedListeningNames)) {
      val stateHash = ProtoUtil.postStateHash(block)
      for {
        continuations <- runtimeManager.getContinuation(stateHash)(sortedListeningNames)
        continuationInfos = continuations.map(
          continuation => WaitingContinuationInfo(continuation._1, continuation._2)
        )
        blockInfo <- getLightBlockInfo(block)
      } yield Option[ContinuationsWithBlockInfo](
        ContinuationsWithBlockInfo(continuationInfos, blockInfo)
      )
    } else {
      none[ContinuationsWithBlockInfo].pure[F]
    }

  private def isListeningNameReduced(
      block: BlockMessage,
      sortedListeningName: Seq[Par]
  ): Boolean = {
    val serializedLog = for {
      pd    <- block.body.deploys
      event <- pd.deployLog
    } yield event
    val log =
      serializedLog.map(EventConverter.toRspaceEvent)
    log.exists {
      case Produce(channelHash, _, _) =>
        assert(sortedListeningName.size == 1, "Produce can have only one channel")
        channelHash == StableHashProvider.hash(sortedListeningName.head)
      case Consume(channelsHashes, _, _) =>
        channelsHashes.toList.sorted == sortedListeningName
          .map(StableHashProvider.hash(_))
          .toList
          .sorted
      case COMM(consume, produces, _, _) =>
        (consume.channelsHashes.toList.sorted ==
          sortedListeningName.map(StableHashProvider.hash(_)).toList.sorted) ||
          produces.exists(
            produce => produce.channelsHash == StableHashProvider.hash(sortedListeningName)
          )
    }
  }

  private def toposortDag[A](depth: Int, maxDepthLimit: Int)(
      doIt: Vector[Vector[BlockHash]] => F[ApiErr[A]]
  ): F[ApiErr[A]] = {
    def response: F[ApiErr[A]] =
      for {
        dag               <- dagStorage.getRepresentation
        latestBlockNumber = dag.latestBlockNumber
        topoSort          <- dag.topoSortUnsafe(latestBlockNumber - depth, none)
        result            <- doIt(topoSort)
      } yield result

    if (depth > maxDepthLimit)
      s"Your request depth $depth exceed the max limit $maxDepthLimit".asLeft[A].pure[F]
    else
      response
  }

  override def getBlocksByHeights(
      startBlockNumber: Long,
      endBlockNumber: Long,
      maxBlocksLimit: Int
  ): F[ApiErr[List[LightBlockInfo]]] = {
    def response: F[ApiErr[List[LightBlockInfo]]] =
      for {
        dag         <- dagStorage.getRepresentation
        topoSortDag <- dag.topoSortUnsafe(startBlockNumber, Some(endBlockNumber))
        result <- topoSortDag
                   .foldM(List.empty[LightBlockInfo]) {
                     case (blockInfosAtHeightAcc, blockHashesAtHeight) =>
                       for {
                         blocksAtHeight     <- blockHashesAtHeight.traverse(BlockStore[F].getUnsafe)
                         blockInfosAtHeight <- blocksAtHeight.traverse(getLightBlockInfo)
                       } yield blockInfosAtHeightAcc ++ blockInfosAtHeight
                   }
                   .map(_.asRight[Error])
      } yield result

    if (endBlockNumber - startBlockNumber > maxBlocksLimit)
      s"Your request startBlockNumber $startBlockNumber and endBlockNumber $endBlockNumber exceed the max limit $maxBlocksLimit"
        .asLeft[List[LightBlockInfo]]
        .pure[F]
    else
      response
  }

  override def visualizeDag[R](
      depth: Int,
      maxDepthLimit: Int,
      startBlockNumber: Int,
      visualizer: (Vector[Vector[BlockHash]], String) => F[Graphz[F]],
      serialize: F[R]
  ): F[ApiErr[R]] =
    for {
      dag <- dagStorage.getRepresentation
      // the default startBlockNumber is 0
      // if the startBlockNumber is 0 , it would use the latestBlockNumber for backward compatible
      startBlockNum = if (startBlockNumber == 0) dag.latestBlockNumber else startBlockNumber.toLong
      topoSortDag <- dag.topoSortUnsafe(
                      startBlockNum - depth,
                      Some(startBlockNum)
                    )
      _      <- visualizer(topoSortDag, PrettyPrinter.buildString(dag.lastFinalizedBlockHash))
      result <- serialize
    } yield result.asRight[Error]

  override def machineVerifiableDag(depth: Int, maxDepthLimit: Int): F[ApiErr[String]] =
    toposortDag[String](depth, maxDepthLimit) { topoSort =>
      val fetchParents: BlockHash => F[List[BlockHash]] = { blockHash =>
        BlockStore[F].getUnsafe(blockHash) map (_.header.parentsHashList)
      }

      MachineVerifiableDag[F](topoSort, fetchParents)
        .map(_.map(edges => edges.show).mkString("\n"))
        .map(_.asRight[Error])
    }

  override def getBlocks(depth: Int, maxDepthLimit: Int): F[ApiErr[List[LightBlockInfo]]] =
    toposortDag[List[LightBlockInfo]](depth, maxDepthLimit) { topoSort =>
      topoSort
        .foldM(List.empty[LightBlockInfo]) {
          case (blockInfosAtHeightAcc, blockHashesAtHeight) =>
            for {
              blocksAtHeight <- blockHashesAtHeight.traverse(BlockStore[F].getUnsafe)
              blockInfosAtHeight <- blocksAtHeight.traverse(
                                     getLightBlockInfo
                                   )
            } yield blockInfosAtHeightAcc ++ blockInfosAtHeight
        }
        .map(_.reverse.asRight[Error])
    }

  override def findDeploy(id: DeployId): F[ApiErr[LightBlockInfo]] =
    for {
      dag            <- dagStorage.getRepresentation
      maybeBlockHash <- BlockDagStorage[F].lookupByDeployId(id)
      maybeBlock     <- maybeBlockHash.traverse(BlockStore[F].getUnsafe)
      response       <- maybeBlock.traverse(getLightBlockInfo)
    } yield response.fold(
      s"Couldn't find block containing deploy with id: ${PrettyPrinter
        .buildStringNoLimit(id)}".asLeft[LightBlockInfo]
    )(_.asRight)

  override def getBlock(hash: String): F[ApiErr[BlockInfo]] = Span[F].trace(getBlockSource) {
    val response = for {
      // Add constraint on the length of searched hash to prevent to many block results
      // which can cause severe CPU load.
      _ <- BlockRetrievalError(s"Input hash value must be at least 6 characters: $hash")
            .raiseError[F, ApiErr[BlockInfo]]
            .whenA(hash.length < 6)
      // Check if hash string is in Base16 encoding and convert to ByteString
      hashByteString <- hash.hexToByteString
                         .liftTo[F](
                           BlockRetrievalError(
                             s"Input hash value is not valid hex string: $hash"
                           )
                         )
      // Check if hash is complete and not just the prefix in which case
      // we can use `get` directly and not iterate over the whole block hash index.
      getBlock  = BlockStore[F].get1(hashByteString)
      findBlock = findBlockFromStore(hash)
      blockOpt  <- if (hash.length == 64) getBlock else findBlock
      // Get block form the block store
      block <- blockOpt.liftTo(
                BlockRetrievalError(s"Error: Failure to find block with hash: $hash")
              )
      // Check if the block is added to the dag and convert it to block info
      dag <- dagStorage.getRepresentation
      blockInfo <- if (dag.contains(block.blockHash)) getFullBlockInfo(block)
                  else
                    BlockRetrievalError(s"Error: Block with hash $hash received but not added yet")
                      .raiseError[F, BlockInfo]

    } yield blockInfo

    response.map(_.asRight[String]).handleError {
      // Convert error message from BlockRetrievalError
      case BlockRetrievalError(errorMessage) => errorMessage.asLeft[BlockInfo]
    }
  }

  private def getBlockInfo[A](
      block: BlockMessage,
      constructor: (BlockMessage, Float) => A
  ): F[A] = constructor(block, -1f).pure[F]

  private def getFullBlockInfo(
      block: BlockMessage
  ): F[BlockInfo] =
    getBlockInfo[BlockInfo](block, constructBlockInfo)

  override def getLightBlockInfo(block: BlockMessage): F[LightBlockInfo] =
    getBlockInfo[LightBlockInfo](block, constructLightBlockInfo)

  private def constructBlockInfo(
      block: BlockMessage,
      faultTolerance: Float
  ): BlockInfo = {
    val lightBlockInfo = constructLightBlockInfo(block, faultTolerance)
    val deploys        = block.body.deploys.map(_.toDeployInfo)
    BlockInfo(blockInfo = lightBlockInfo, deploys = deploys)
  }

  private def constructLightBlockInfo(
      block: BlockMessage,
      faultTolerance: Float
  ): LightBlockInfo =
    LightBlockInfo(
      blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
      sender = PrettyPrinter.buildStringNoLimit(block.sender),
      seqNum = block.seqNum.toLong,
      sig = PrettyPrinter.buildStringNoLimit(block.sig),
      sigAlgorithm = block.sigAlgorithm,
      shardId = block.shardId,
      extraBytes = block.extraBytes,
      version = block.header.version,
      timestamp = block.header.timestamp,
      headerExtraBytes = block.header.extraBytes,
      parentsHashList = block.header.parentsHashList.map(PrettyPrinter.buildStringNoLimit),
      blockNumber = block.body.state.blockNumber,
      preStateHash = PrettyPrinter.buildStringNoLimit(block.body.state.preStateHash),
      postStateHash = PrettyPrinter.buildStringNoLimit(block.body.state.postStateHash),
      bodyExtraBytes = block.body.extraBytes,
      bonds = block.body.state.bonds.map(ProtoUtil.bondToBondInfo),
      blockSize = block.toProto.serializedSize.toString,
      deployCount = block.body.deploys.length,
      faultTolerance = faultTolerance,
      justifications = block.justifications.map(ProtoUtil.justificationsToJustificationInfos),
      rejectedDeploys = block.body.rejectedDeploys.map(
        r => RejectedDeployInfo(PrettyPrinter.buildStringNoLimit(r.sig))
      )
    )

  // Be careful to use this method , because it would iterate the whole indexes to find the matched one which would cause performance problem
  // Trying to use BlockStore.get as much as possible would more be preferred
  private def findBlockFromStore(hash: String): F[Option[BlockMessage]] =
    for {
      dag          <- dagStorage.getRepresentation
      blockHashOpt = dag.find(hash)
      message      <- blockHashOpt.flatTraverse(BlockStore[F].get1)
    } yield message

  override def previewPrivateNames(
      deployer: ByteString,
      timestamp: Long,
      nameQty: Int
  ): F[ApiErr[Seq[ByteString]]] = {
    val rand                = Tools.unforgeableNameRng(PublicKey(deployer.toByteArray), timestamp)
    val safeQty             = nameQty min 1024
    val ids: Seq[BlockHash] = (0 until safeQty).map(_ => ByteString.copyFrom(rand.next()))
    ids.asRight[String].pure[F]
  }

  override def lastFinalizedBlock: F[ApiErr[BlockInfo]] =
    for {
      dag                <- dagStorage.getRepresentation
      lastFinalizedBlock <- dag.lastFinalizedBlockUnsafe.flatMap(BlockStore[F].getUnsafe)
      blockInfo          <- getFullBlockInfo(lastFinalizedBlock)
    } yield blockInfo.asRight

  override def isFinalized(hash: String): F[ApiErr[Boolean]] =
    for {
      dag            <- dagStorage.getRepresentation
      givenBlockHash = hash.unsafeHexToByteString
      result         = dag.isFinalized(givenBlockHash)
    } yield result.asRight[Error]

  override def bondStatus(
      publicKey: ByteString,
      targetBlock: Option[BlockMessage] = none[BlockMessage]
  ): F[ApiErr[Boolean]] =
    for {
      dag                <- dagStorage.getRepresentation
      lastFinalizedBlock <- dag.lastFinalizedBlockUnsafe.flatMap(BlockStore[F].getUnsafe)
      postStateHash      = ProtoUtil.postStateHash(targetBlock.getOrElse(lastFinalizedBlock))
      bonds              <- runtimeManager.computeBonds(postStateHash)
      validatorBondOpt   = bonds.find(_.validator == publicKey)
    } yield validatorBondOpt.isDefined.asRight[Error]

  /**
    * Explore the data or continuation in the tuple space for specific blockHash
    *
    * @param term: the term you want to explore in the request. Be sure the first new should be `return`
    * @param blockHash: the block hash you want to explore
    * @param usePreStateHash: Each block has preStateHash and postStateHash. If usePreStateHash is true, the explore
    *                       would try to execute on preState.
    * */
  override def exploratoryDeploy(
      term: String,
      blockHash: Option[String] = none,
      usePreStateHash: Boolean = false,
      devMode: Boolean = false
  ): F[ApiErr[(Seq[Par], LightBlockInfo)]] =
    for {
      isReadOnly <- Sync[F].delay(validatorOpt.isEmpty)
      result <- if (isReadOnly || devMode) {
                 for {
                   dag <- dagStorage.getRepresentation
                   targetBlock <- if (blockHash.isEmpty)
                                   dag.lastFinalizedBlockUnsafe.flatMap(BlockStore[F].get1)
                                 else
                                   for {
                                     hashByteString <- blockHash
                                                        .getOrElse("")
                                                        .hexToByteString
                                                        .liftTo[F](
                                                          BlockRetrievalError(
                                                            s"Input hash value is not valid hex string: $blockHash"
                                                          )
                                                        )
                                     block <- BlockStore[F].get1(hashByteString)
                                   } yield block
                   res <- targetBlock.traverse(b => {
                           val postStateHash =
                             if (usePreStateHash) ProtoUtil.preStateHash(b)
                             else ProtoUtil.postStateHash(b)
                           for {
                             res            <- runtimeManager.playExploratoryDeploy(term, postStateHash)
                             lightBlockInfo <- getLightBlockInfo(b)
                           } yield (res, lightBlockInfo)
                         })
                 } yield res.fold(
                   s"Can not find block $blockHash".asLeft[(Seq[Par], LightBlockInfo)]
                 )(_.asRight[Error])
               } else {
                 "Exploratory deploy can only be executed on read-only RNode."
                   .asLeft[(Seq[Par], LightBlockInfo)]
                   .pure[F]
               }
    } yield result

  override def getLatestMessage: F[ApiErr[BlockMetadata]] =
    for {
      validator        <- validatorOpt.liftTo[F](ValidatorReadOnlyError)
      dag              <- dagStorage.getRepresentation
      latestMessageOpt <- dag.latestMessage(ByteString.copyFrom(validator.publicKey.bytes))
      latestMessage    <- latestMessageOpt.liftTo[F](NoBlockMessageError)
    } yield latestMessage.asRight[Error]

  override def getDataAtPar(
      par: Par,
      blockHash: String,
      usePreStateHash: Boolean
  ): F[ApiErr[(Seq[Par], LightBlockInfo)]] =
    for {
      block     <- BlockStore[F].getUnsafe(blockHash.unsafeHexToByteString)
      sortedPar <- parSortable.sortMatch[F](par).map(_.term)
      data      <- getDataWithBlockInfo(sortedPar, block).map(_.get)
    } yield (data.postBlockData, data.block).asRight[Error]
}
