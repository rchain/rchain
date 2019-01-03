package coop.rchain.casper.api

import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.casper._, Estimator.BlockHash, MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.graphz._
import coop.rchain.shared.Log

import cats.Monad
import cats.effect.Sync
import cats._, cats.data._, cats.implicits._
import cats.mtl._
import cats.mtl.implicits._
import coop.rchain.catscontrib.ski._

case class ValidatorBlock(
    blockHash: String,
    parentsHashes: List[String],
    justifications: List[String]
)

object ValidatorBlock {
  implicit val validatorBlockMonoid: Monoid[ValidatorBlock] = new Monoid[ValidatorBlock] {
    def empty: ValidatorBlock = ValidatorBlock("", List.empty[String], List.empty[String])
    def combine(vb1: ValidatorBlock, vb2: ValidatorBlock): ValidatorBlock =
      ValidatorBlock(
        vb2.blockHash,
        vb1.parentsHashes ++ vb2.parentsHashes,
        vb1.justifications ++ vb2.justifications
      )
  }
}

case class GraphConfig(showJustificationLines: Boolean = false)

object GraphzGenerator {

  type ValidatorsBlocks = Map[Long, ValidatorBlock]

  case class DagInfo[G[_]](
      validators: Map[String, ValidatorsBlocks] = Map.empty,
      timeseries: List[Long] = List.empty
  )

  object DagInfo {
    def empty[G[_]]: DagInfo[G] = DagInfo[G]()
  }

  def dagAsCluster[
      F[_]: Monad: Sync: MultiParentCasperRef: Log: SafetyOracle: BlockStore,
      G[_]: Monad: GraphSerializer
  ](
      topoSort: Vector[Vector[BlockHash]],
      lastFinalizedBlockHash: String,
      config: GraphConfig
  ): F[G[Graphz[G]]] =
    for {
      acc <- topoSort.foldM(DagInfo.empty[G])(accumulateDagInfo[F, G](_, _))
    } yield {

      val timeseries = acc.timeseries.reverse
      val firstTs    = timeseries.head
      val validators = acc.validators
      for {
        g <- initGraph[G]("dag")
        allAncestors = validators.toList
          .flatMap {
            case (_, blocks) =>
              blocks.get(firstTs).map(_.parentsHashes).getOrElse(List.empty[String])
          }
          .toSet
          .toList
        // draw ancesotrs first
        _ <- allAncestors.traverse(
              ancestor =>
                g.node(
                  ancestor,
                  style = styleFor(ancestor, lastFinalizedBlockHash),
                  shape = Box
                )
            )
        // create invisible edges from ancestors to first node in each cluster for proper alligment
        _ <- validators.toList.traverse {
              case (id, blocks) =>
                allAncestors.traverse(ancestor => {
                  val node = nodeForTs(id, firstTs, blocks, lastFinalizedBlockHash)._2
                  g.edge(ancestor, node, style = Some(Invis))
                })
            }
        // draw clusters per validator
        _ <- validators.toList.traverse {
              case (id, blocks) =>
                g.subgraph(
                  validatorCluster(id, blocks, timeseries, lastFinalizedBlockHash)
                )
            }
        // draw justification dotted lines
        _ <- validators.values.toList.flatMap(_.values.toList).traverse {
              case ValidatorBlock(blockHash, parentsHashes, justifications) => {
                val parentsEdges = parentsHashes
                  .traverse(p => g.edge(blockHash, p, constraint = Some(false)))
                val justificationsEdges = justifications
                  .traverse(
                    j =>
                      g.edge(
                        blockHash,
                        j,
                        style = Some(Dotted),
                        constraint = Some(false),
                        arrowHead = Some(NoneArrow)
                      )
                  )
                parentsEdges *> justificationsEdges
              }

            }
        _ <- g.close
      } yield g

    }

  private def accumulateDagInfo[
      F[_]: Monad: Sync: MultiParentCasperRef: Log: SafetyOracle: BlockStore,
      G[_]
  ](
      acc: DagInfo[G],
      blockHashes: Vector[BlockHash]
  ): F[DagInfo[G]] =
    for {
      blocks    <- blockHashes.traverse(ProtoUtil.unsafeGetBlock[F])
      timeEntry = blocks.head.getBody.getState.blockNumber
      validators = blocks.toList.map {
        case b =>
          val blockHash       = PrettyPrinter.buildString(b.blockHash)
          val blockSenderHash = PrettyPrinter.buildString(b.sender)
          val parents = b.getHeader.parentsHashList.toList
            .map(PrettyPrinter.buildString)
          val justifications = b.justifications
            .map(_.latestBlockHash)
            .map(PrettyPrinter.buildString)
            .toSet
            .toList
          val validatorBlocks =
            Map(timeEntry -> ValidatorBlock(blockHash, parents, justifications))
          Map(blockSenderHash -> validatorBlocks)
      }
    } yield
      acc.copy(
        timeseries = timeEntry :: acc.timeseries,
        validators = acc.validators |+| Foldable[List].fold(validators)
      )

  private def initGraph[G[_]: Monad: GraphSerializer](name: String): G[Graphz[G]] =
    Graphz[G](
      name,
      DiGraph,
      rankdir = Some(BT),
      node = Map("width" -> "0", "height" -> "0", "margin" -> "0.03", "fontsize" -> "8")
    )

  private def nodeForTs(
      validatorId: String,
      ts: Long,
      blocks: ValidatorsBlocks,
      lastFinalizedBlockHash: String
  ): (Option[GraphStyle], String) =
    blocks.get(ts) match {
      case Some(ValidatorBlock(blockHash, _, _)) =>
        (styleFor(blockHash, lastFinalizedBlockHash), blockHash)
      case None => (Some(Invis), s"${ts.show}_$validatorId")
    }

  private def validatorCluster[G[_]: Monad: GraphSerializer](
      id: String,
      blocks: ValidatorsBlocks,
      timeseries: List[Long],
      lastFinalizedBlockHash: String
  ): G[Graphz[G]] =
    for {
      g     <- Graphz.subgraph[G](s"cluster_$id", DiGraph, label = Some(id))
      nodes = timeseries.map(ts => nodeForTs(id, ts, blocks, lastFinalizedBlockHash))
      _ <- nodes.traverse {
            case (style, name) => g.node(name, style = style, shape = Box)
          }
      _ <- nodes.zip(nodes.drop(1)).traverse {
            case ((_, n1), (_, n2)) => g.edge(n1, n2, style = Some(Invis))
          }
      _ <- g.close
    } yield g

  private def styleFor(blockHash: String, lastFinalizedBlockHash: String): Option[GraphStyle] =
    if (blockHash == lastFinalizedBlockHash) Some(Filled) else None

}
