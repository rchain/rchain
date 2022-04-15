package coop.rchain.casper.api

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.api.BlockAPI_v2
import coop.rchain.blockstorage.BlockStore
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
import coop.rchain.models.Validator.Validator
import coop.rchain.models.rholang.sorter.Sortable._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockMetadata, NormalizerEnv, Par}
import coop.rchain.rspace.hashing.StableHashProvider
import coop.rchain.rspace.trace._
import coop.rchain.shared.Log

import scala.collection.immutable

class BlockAPIImpl[F[_]: Concurrent: Log: Span: SafetyOracle: BlockStore: DeployStorage: MultiParentCasper](
    runtimeManager: RuntimeManager[F],
    dagStorage: BlockDagStorage[F],
    validatorPrivateKey: Option[String]
) extends BlockAPI_v2[F] {

  val BlockAPIMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "block-api")
  val DeploySource: Metrics.Source          = Metrics.Source(BlockAPIMetricsSource, "deploy")

  override def deploy(
      d: Signed[DeployData],
      triggerPropose: Option[ProposeFunction[F]],
      minPhloPrice: Long,
      isNodeReadOnly: Boolean,
      shardId: Error
  ): F[ApiErr[Error]] = Span[F].trace(DeploySource) {

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

  override def getListeningNameDataResponse(
      depth: Int,
      listeningName: Par,
      maxBlocksLimit: Int
  ): F[ApiErr[(Seq[DataWithBlockInfo], Int)]] =
    if (depth > maxBlocksLimit)
      s"Your request on getListeningName depth $depth exceed the max limit $maxBlocksLimit"
        .asLeft[(Seq[DataWithBlockInfo], Int)]
        .pure[F]
    else
      for {
        mainChain           <- getMainChainFromTip(depth)
        sortedListeningName <- parSortable.sortMatch[F](listeningName).map(_.term)
        maybeBlocksWithActiveName <- mainChain.toList.traverse { block =>
                                      getDataWithBlockInfo(sortedListeningName, block)
                                    }
        blocksWithActiveName = maybeBlocksWithActiveName.flatten
      } yield (blocksWithActiveName, blocksWithActiveName.length).asRight

  override def getListeningNameContinuationResponse(
      depth: Int,
      listeningNames: Seq[Par],
      maxBlocksLimit: Int
  ): F[ApiErr[(Seq[ContinuationsWithBlockInfo], Int)]] =
    if (depth > maxBlocksLimit)
      s"Your request on getListeningNameContinuation depth $depth exceed the max limit $maxBlocksLimit"
        .asLeft[(Seq[ContinuationsWithBlockInfo], Int)]
        .pure[F]
    else
      for {
        mainChain <- getMainChainFromTip(depth)
        sortedListeningNames <- listeningNames.toList
                                 .traverse(parSortable.sortMatch[F](_).map(_.term))
        maybeBlocksWithActiveName <- mainChain.toList.traverse { block =>
                                      getContinuationsWithBlockInfo(
                                        sortedListeningNames,
                                        block
                                      )
                                    }
        blocksWithActiveName = maybeBlocksWithActiveName.flatten
      } yield (blocksWithActiveName, blocksWithActiveName.length).asRight

  private def getMainChainFromTip(depth: Int): F[IndexedSeq[BlockMessage]] =
    for {
      tipHashes <- estimator
      tipHash   = tipHashes.head
      tip       <- BlockStore[F].getUnsafe(tipHash)
      mainChain <- ProtoUtil.getMainChainUntilDepth[F](tip, IndexedSeq.empty[BlockMessage], depth)
    } yield mainChain

  private def estimator: F[IndexedSeq[BlockHash]] = IndexedSeq.empty[BlockHash].pure

  private def getDataWithBlockInfo(
      sortedListeningName: Par,
      block: BlockMessage
  ): F[Option[DataWithBlockInfo]] =
    // TODO: For Produce it doesn't make sense to have multiple names
    if (isListeningNameReduced(block, immutable.Seq(sortedListeningName))) {
      val stateHash = ProtoUtil.postStateHash(block)
      for {
        data      <- runtimeManager.getData(stateHash)(sortedListeningName)
        blockInfo <- getLightBlockInfo(block)
      } yield Option[DataWithBlockInfo](DataWithBlockInfo(data, blockInfo))
    } else {
      none[DataWithBlockInfo].pure[F]
    }

  private def getContinuationsWithBlockInfo(
      sortedListeningNames: immutable.Seq[Par],
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
      sortedListeningName: immutable.Seq[Par]
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
  ): F[ApiErr[A]] =
    if (depth > maxDepthLimit)
      s"Your request depth $depth exceed the max limit $maxDepthLimit".asLeft[A].pure[F]
    else
      for {
        dag               <- dagStorage.getRepresentation
        latestBlockNumber <- dag.latestBlockNumber
        topoSort          <- dag.topoSort(latestBlockNumber - depth, none)
        result            <- doIt(topoSort)
      } yield result

  override def getBlocksByHeights(
      startBlockNumber: Long,
      endBlockNumber: Long,
      maxBlocksLimit: Int
  ): F[ApiErr[List[LightBlockInfo]]] =
    if (endBlockNumber - startBlockNumber > maxBlocksLimit)
      s"Your request startBlockNumber $startBlockNumber and endBlockNumber $endBlockNumber exceed the max limit $maxBlocksLimit"
        .asLeft[List[LightBlockInfo]]
        .pure[F]
    else
      for {
        dag         <- dagStorage.getRepresentation
        topoSortDag <- dag.topoSort(startBlockNumber, Some(endBlockNumber))
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
      startBlockNum <- if (startBlockNumber == 0) dag.latestBlockNumber
                      else Sync[F].delay(startBlockNumber.toLong)
      topoSortDag <- dag.topoSort(
                      startBlockNum - depth,
                      Some(startBlockNum)
                    )
      _      <- visualizer(topoSortDag, PrettyPrinter.buildString(dag.lastFinalizedBlock))
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

  override def showMainChain(depth: Int, maxDepthLimit: Int): F[List[LightBlockInfo]] =
    if (depth > maxDepthLimit)
      List.empty[LightBlockInfo].pure[F]
    else
      for {
        tipHashes  <- estimator
        tipHash    = tipHashes.head
        tip        <- BlockStore[F].getUnsafe(tipHash)
        mainChain  <- ProtoUtil.getMainChainUntilDepth[F](tip, IndexedSeq.empty[BlockMessage], depth)
        blockInfos <- mainChain.toList.traverse(getLightBlockInfo)
      } yield blockInfos

  override def findDeploy(id: DeployId): F[ApiErr[LightBlockInfo]] =
    for {
      dag            <- dagStorage.getRepresentation
      maybeBlockHash <- dag.lookupByDeployId(id)
      maybeBlock     <- maybeBlockHash.traverse(BlockStore[F].getUnsafe)
      response       <- maybeBlock.traverse(getLightBlockInfo)
    } yield response.fold(
      s"Couldn't find block containing deploy with id: ${PrettyPrinter
        .buildStringNoLimit(id)}".asLeft[LightBlockInfo]
    )(
      _.asRight
    )

  override def getBlock(hash: String): F[ApiErr[BlockInfo]] =
    (for {
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
      getBlock  = BlockStore[F].get(hashByteString)
      findBlock = findBlockFromStore(hash)
      blockF    = if (hash.length == 64) getBlock else findBlock
      // Get block form the block store
      block <- blockF >>= (_.liftTo[F](
                BlockRetrievalError(s"Error: Failure to find block with hash: $hash")
              ))
      // Check if the block is added to the dag and convert it to block info
      dag <- dagStorage.getRepresentation
      blockInfo <- dag
                    .contains(block.blockHash)
                    .ifM(
                      getFullBlockInfo(block),
                      BlockRetrievalError(
                        s"Error: Block with hash $hash received but not added yet"
                      ).raiseError[F, BlockInfo]
                    )
    } yield blockInfo.asRight).map(_.asInstanceOf[ApiErr[BlockInfo]]).handleError {
      // Convert error message from BlockRetrievalError
      case BlockRetrievalError(errorMessage) => errorMessage.asLeft[BlockInfo]
    }

  private def getBlockInfo[A](block: BlockMessage, constructor: (BlockMessage, Float) => A): F[A] =
    for {
      dag <- dagStorage.getRepresentation
      // TODO this is temporary solution to not calculate fault tolerance all the blocks
      oldBlock = dag.latestBlockNumber.map(_ - block.body.state.blockNumber).map(_ > 100)

      // Old block fault tolerance / invalid block has -1.0 fault tolerance
      normalizedFaultTolerance <- oldBlock.ifM(
                                   dag
                                     .isFinalized(block.blockHash)
                                     .map(isFinalized => if (isFinalized) 1f else -1f),
                                   SafetyOracle[F]
                                     .normalizedFaultTolerance(dag, block.blockHash)
                                 )
      initialFault   <- normalizedInitialFault(ProtoUtil.weightMap(block))
      faultTolerance = normalizedFaultTolerance - initialFault

      blockInfo = constructor(block, faultTolerance)
    } yield blockInfo

  private def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    dagStorage.accessEquivocationsTracker { tracker =>
      tracker.equivocationRecords.map { equivocations =>
        equivocations
          .map(_.equivocator)
          .flatMap(weights.get)
          .sum
          .toFloat / ProtoUtil.weightMapTotal(weights)
      }
    }

  private def getFullBlockInfo(block: BlockMessage): F[BlockInfo] =
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
      blockHashOpt <- dag.find(hash)
      message      <- blockHashOpt.flatTraverse(BlockStore[F].get)
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
      lastFinalizedBlock <- BlockStore[F].getUnsafe(dag.lastFinalizedBlock)
      blockInfo          <- getFullBlockInfo(lastFinalizedBlock)
    } yield blockInfo.asRight

  override def isFinalized(hash: String): F[ApiErr[Boolean]] =
    for {
      dag            <- dagStorage.getRepresentation
      givenBlockHash = hash.unsafeHexToByteString
      result         <- dag.isFinalized(givenBlockHash)
    } yield result.asRight[Error]

  override def bondStatus(publicKey: ByteString): F[ApiErr[Boolean]] =
    for {
      dag                <- dagStorage.getRepresentation
      lastFinalizedBlock <- BlockStore[F].getUnsafe(dag.lastFinalizedBlock)
      postStateHash      = ProtoUtil.postStateHash(lastFinalizedBlock)
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
      isReadOnly <- getValidator.map(_.isEmpty)
      result <- if (isReadOnly || devMode) {
                 for {
                   dag <- dagStorage.getRepresentation
                   targetBlock <- if (blockHash.isEmpty)
                                   BlockStore[F].get(dag.lastFinalizedBlock)
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
                                     block <- BlockStore[F].get(hashByteString)
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
      validatorOpt     <- getValidator
      validator        <- validatorOpt.liftTo[F](ValidatorReadOnlyError)
      dag              <- dagStorage.getRepresentation
      latestMessageOpt <- dag.latestMessage(ByteString.copyFrom(validator.publicKey.bytes))
      latestMessage    <- latestMessageOpt.liftTo[F](NoBlockMessageError)
    } yield latestMessage.asRight[Error]

  private def getValidator: F[Option[ValidatorIdentity]] =
    ValidatorIdentity.fromPrivateKeyWithLogging[F](validatorPrivateKey)

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