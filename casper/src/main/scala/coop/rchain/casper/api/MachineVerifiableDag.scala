package coop.rchain.casper.api

import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.codec.Base16
import cats._, cats.data._, cats.implicits._

final case class VerifiableEdge(from: String, to: String)

object VerifiableEdge {

  implicit def showBlockHash: Show[BlockHash] = new Show[BlockHash] {
    def show(blockHash: BlockHash): String =
      Base16.encode(blockHash.toByteArray)
  }

  implicit def showVerifiableEdge: Show[VerifiableEdge] = new Show[VerifiableEdge] {
    def show(ve: VerifiableEdge): String = s"${ve.from} - ${ve.to}"
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
          for {
            parents          <- blockHashes.toList.traverse(fetchParents)
            blocks           = blockHashes.toList.map(_.show)
            blocksAndParents = blocks.zip(parents.map(_.map(p => p.show)))
            entries = blocksAndParents.flatMap {
              case (b, bp) => bp.map(VerifiableEdge(b, _))
            }
          } yield entries ++ acc
      }
      .map(_.reverse)
  }

}
