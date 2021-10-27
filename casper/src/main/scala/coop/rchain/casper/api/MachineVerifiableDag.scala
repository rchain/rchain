package coop.rchain.casper.api

import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.models.BlockHash.BlockHash
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.shared.Base16

final case class VerifiableEdge(from: String, to: String)

object VerifiableEdge {

  implicit def showBlockHash: Show[BlockHash] = new Show[BlockHash] {
    def show(blockHash: BlockHash): String =
      Base16.encode(blockHash.toByteArray)
  }

  implicit def showVerifiableEdge: Show[VerifiableEdge] = new Show[VerifiableEdge] {
    def show(ve: VerifiableEdge): String = s"${ve.from} ${ve.to}"
  }
}

object MachineVerifiableDag {
  def apply[F[_]: Monad](
      toposort: TopoSort,
      fetchParents: BlockHash => F[List[BlockHash]]
  ): F[List[VerifiableEdge]] = {
    import VerifiableEdge._

    toposort
      .foldM(List.empty[VerifiableEdge]) {
        case (acc, blockHashes) =>
          blockHashes.toList
            .traverse { blockHash =>
              fetchParents(blockHash).map(parents => (blockHash.show, parents.map(p => p.show)))
            }
            .map { blocksAndParents =>
              blocksAndParents.flatMap {
                case (b, bp) => bp.map(VerifiableEdge(b, _))
              }
            }
            .map(_ ++ acc)

      }
  }
}
