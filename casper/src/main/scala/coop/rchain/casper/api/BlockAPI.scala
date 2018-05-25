package coop.rchain.casper.api

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import coop.rchain.casper.protocol.{BlockInfo, BlockMessage, BlockQuery, MaybeBlockMessage}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.{BlockDag, MultiParentCasper, PrettyPrinter}
import coop.rchain.crypto.codec.Base16

object BlockAPI {
  def createBlock[F[_]: Monad: MultiParentCasper]: F[MaybeBlockMessage] =
    MultiParentCasper[F].createBlock.map(MaybeBlockMessage.apply)

  def addBlock[F[_]: Monad: MultiParentCasper](b: BlockMessage): F[Empty] =
    MultiParentCasper[F].addBlock(b).map(_ => Empty())

  def getBlockInfo[F[_]: Monad: MultiParentCasper](q: BlockQuery): F[BlockInfo] =
    for {
      dag        <- MultiParentCasper[F].blockDag
      maybeBlock = getBlock[F](q, dag)
      blockInfo <- maybeBlock match {
                    case Some(block) => getBlockInfo[F](block)
                    case None =>
                      BlockInfo(status = s"Error: Failure to find block with hash ${q.hash}")
                        .pure[F]
                  }
    } yield blockInfo

  private def getBlockInfo[F[_]: Monad: MultiParentCasper](block: BlockMessage): F[BlockInfo] =
    for {
      parents <- block.header.fold(Seq.empty[ByteString])(_.parentsHashList).pure[F]
      tsHash <- {
        val ps = block.body.flatMap(_.postState)
        ps.fold(ByteString.EMPTY)(_.tuplespace).pure[F]
      }
      tsDesc <- MultiParentCasper[F]
                 .tsCheckpoint(tsHash)
                 .map(maybeCheckPoint => {
                   maybeCheckPoint
                     .map(checkpoint => {
                       val ts     = checkpoint.toTuplespace
                       val result = ts.storageRepr
                       ts.delete()
                       result
                     })
                     .getOrElse(s"Tuplespace hash ${Base16.encode(tsHash.toByteArray)} not found!")
                 })
    } yield {
      BlockInfo(
        status = "Success",
        blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
        blockNumber = ProtoUtil.blockNumber(block),
        parentsHashList = parents.map(PrettyPrinter.buildStringNoLimit),
        tsDesc = tsDesc
      )
    }

  private def getBlock[F[_]: Monad: MultiParentCasper](q: BlockQuery,
                                                       dag: BlockDag): Option[BlockMessage] = {
    val fullHash = dag.blockLookup.keys.find(h => {
      Base16.encode(h.toByteArray).startsWith(q.hash)
    })
    fullHash.map(h => dag.blockLookup(h))
  }
}
