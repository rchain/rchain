package coop.rchain.casper.blocks.merger

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.models.BlockHash.BlockHash

final case class MergingVertex(
    blockHash: BlockHash = ByteString.EMPTY,
    postStateHash: StateHash,
    preStateHash: StateHash = ByteString.EMPTY,
    processedDeploys: Set[ProcessedDeploy]
)
