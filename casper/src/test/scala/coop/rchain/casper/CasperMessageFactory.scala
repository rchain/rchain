package coop.rchain.casper

import coop.rchain.casper.protocol._
import com.google.protobuf.ByteString

/** "Fun" way to create CasperMessage objects for tests
  *  This how things were done with protobufs anyway, so no harm there
  *  It might get deleted in upcoming PRs
  */
object CasperMessageFactory {

  def createBlockMessage(
      blockHash: ByteString = ByteString.EMPTY,
      header: Header = createHeader(),
      body: Body = createBody(),
      justifications: List[Justification] = List.empty,
      shardId: String = "",
      sender: ByteString = ByteString.EMPTY,
      seqNum: Int = 0
  ): BlockMessage =
    BlockMessage(
      blockHash = blockHash,
      header = header,
      body = body,
      justifications = justifications,
      sender = sender,
      seqNum = seqNum,
      sig = ByteString.EMPTY,
      sigAlgorithm = "",
      shardId = shardId
    )

  def createHeader(
      parentHashes: List[ByteString] = List.empty,
      deploysHash: ByteString = ByteString.EMPTY,
      timestamp: Long = 0L
  ): Header =
    Header(
      parentsHashList = parentHashes,
      deploysHash = deploysHash,
      timestamp = timestamp,
      version = 0L,
      deployCount = 0
    )

  def createRChainState(
      preStateHash: ByteString = ByteString.EMPTY,
      postStateHash: ByteString = ByteString.EMPTY,
      bonds: List[Bond] = List.empty,
      blockNumber: Long = 0L
  ): RChainState =
    RChainState(preStateHash, postStateHash, bonds, blockNumber)

  def createBody(
      state: RChainState = createRChainState(),
      deploys: List[ProcessedDeploy] = List.empty
  ): Body =
    Body(state, deploys)

}
