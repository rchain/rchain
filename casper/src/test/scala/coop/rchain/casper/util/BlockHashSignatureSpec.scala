package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.models.blockImplicits._
import org.scalacheck.Prop.{forAll, AnyOperators}
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers.check

class BlockHashSignatureSpec extends FlatSpec {

  implicit val aBlock = arbBlockMessage

  "block hash" should "not match for changed block hash" in {
    implicit val aBlockHash = arbitraryBlockHash
    check {
      forAll { (block: BlockMessage, genBlockHash: ByteString) =>
        val isModified    = genBlockHash != block.blockHash
        val hash          = block.blockHash
        val blockModified = block.copy(blockHash = genBlockHash)
        val hashModified  = ProtoUtil.hashBlock(blockModified)

        // Check hashes are different for modified block
        hashModified !== hash ?= isModified
      }
    }
  }

  "block hash" should "not match for changed sequence number" in {
    check {
      forAll { (block: BlockMessage, genSeqNum: Int) =>
        val isModified    = genSeqNum != block.seqNum
        val hash          = ProtoUtil.hashBlock(block)
        val blockModified = block.copy(seqNum = genSeqNum)
        val hashModified  = ProtoUtil.hashBlock(blockModified)

        // Check hashes are different for modified block
        hashModified !== hash ?= isModified
      }
    }
  }

  "block hash" should "not match for changed block number" in {
    check {
      forAll { (block: BlockMessage, genBlockNumber: Long) =>
        val isModified = genBlockNumber != block.body.state.blockNumber
        val hash       = ProtoUtil.hashBlock(block)
        val blockModified = block.copy(
          body = block.body.copy(state = block.body.state.copy(blockNumber = genBlockNumber))
        )
        val hashModified = ProtoUtil.hashBlock(blockModified)

        // Check hashes are different for modified block
        hashModified !== hash ?= isModified
      }
    }
  }

  "block hash" should "not match for changed justifications" in {
    implicit val aJustification = arbitraryJustification
    check {
      forAll { (block: BlockMessage, genJustifications: List[Justification]) =>
        val isModified    = genJustifications != block.justifications
        val hash          = ProtoUtil.hashBlock(block)
        val blockModified = block.copy(justifications = genJustifications)
        val hashModified  = ProtoUtil.hashBlock(blockModified)

        // Check hashes are different for modified block
        hashModified !== hash ?= isModified
      }
    }
  }

  // TODO: add more tests when BlockMessage changes from BM branch are merged
}
