package coop.rchain.casper.api

import cats.{Monad, _}
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.syntax._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.graphz._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log

final case class ValidatorBlock(
    blockHash: String,
    parentsHashes: List[String],
    justifications: List[String]
)

final case class GraphConfig(showJustificationLines: Boolean = false)

object GraphzGenerator {

  type ValidatorsBlocks = Map[Long, List[ValidatorBlock]]

  final case class DagInfo[G[_]](
      validators: Map[String, ValidatorsBlocks] = Map.empty,
      timeseries: List[Long] = List.empty
  )

  object DagInfo {
    def empty[G[_]]: DagInfo[G] = DagInfo[G]()
  }

  def dagAsCluster[F[_]: Monad: Sync: Log: BlockStore, G[_]: Monad: GraphSerializer](
      topoSort: Vector[Vector[BlockHash]],
      finalizedFringe: Set[String],
      base: String,
      config: GraphConfig
  ): F[G[Graphz[G]]] =
    for {
      acc <- topoSort.foldM(DagInfo.empty[G])(accumulateDagInfo[F, G](_, _))
    } yield {

      val timeseries     = acc.timeseries.reverse
      val firstTs        = timeseries.head
      val validators     = acc.validators
      val validatorsList = validators.toList.sortBy(_._1)
      for {
        g <- initGraph[G]("dag")
        allAncestors = validatorsList
          .flatMap {
            case (_, blocks) =>
              blocks.get(firstTs).map(_.flatMap(b => b.parentsHashes)).getOrElse(List.empty[String])
          }
          .distinct
          .sorted
        // draw ancesotrs first
        _ <- allAncestors.traverse(
              ancestor =>
                g.node(
                  ancestor,
                  style = styleFor(ancestor, finalizedFringe, base),
                  shape = Box
                )
            )
        // create invisible edges from ancestors to first node in each cluster for proper alligment
        _ <- validatorsList.traverse {
              case (id, blocks) =>
                allAncestors.traverse(ancestor => {
                  val nodes = nodesForTs(id, firstTs, blocks, finalizedFringe, base).keys.toList
                  nodes.traverse(node => g.edge(ancestor, node, style = Some(Invis)))
                })
            }
        // draw clusters per validator
        _ <- validatorsList.traverse {
              case (id, blocks) =>
                g.subgraph(
                  validatorCluster(id, blocks, timeseries, finalizedFringe, base)
                )
            }
        // draw parent dependencies
        _ <- drawParentDependencies[G](g, validatorsList.map(_._2))
        // draw justification dotted lines
        _ <- config.showJustificationLines.fold(
              drawJustificationDottedLines[G](g, validators),
              ().pure[G]
            )
        _ <- g.close
      } yield g

    }

  private def accumulateDagInfo[F[_]: Monad: Sync: Log: BlockStore, G[_]](
      acc: DagInfo[G],
      blockHashes: Vector[BlockHash]
  ): F[DagInfo[G]] =
    for {
      blocks    <- blockHashes.traverse(BlockStore[F].getUnsafe)
      timeEntry = blocks.head.body.state.blockNumber
      validators = blocks.toList.map { b =>
        val blockHash       = PrettyPrinter.buildString(b.blockHash)
        val blockSenderHash = PrettyPrinter.buildString(b.sender)
        val parents = b.justifications
          .map(_.latestBlockHash)
          .toList
          .map(PrettyPrinter.buildString)
        val justifications = b.justifications
          .map(_.latestBlockHash)
          .map(PrettyPrinter.buildString)
          .toSet
          .toList
        val validatorBlocks =
          Map(timeEntry -> List(ValidatorBlock(blockHash, parents, justifications)))
        Map(blockSenderHash -> validatorBlocks)
      }
    } yield acc.copy(
      timeseries = timeEntry :: acc.timeseries,
      validators = acc.validators |+| Foldable[List].fold(validators)
    )

  private def initGraph[G[_]: Monad: GraphSerializer](name: String): G[Graphz[G]] =
    Graphz[G](
      name,
      DiGraph,
      rankdir = Some(BT),
      splines = Some("false"),
      node = Map("width" -> "0", "height" -> "0", "margin" -> "0.03", "fontsize" -> "8")
    )

  private def drawParentDependencies[G[_]: Applicative](
      g: Graphz[G],
      validators: List[ValidatorsBlocks]
  ): G[Unit] =
    validators
      .flatMap(_.values.toList.flatten)
      .traverse {
        case ValidatorBlock(blockHash, parentsHashes, _) => {
          parentsHashes
            .traverse(p => g.edge(blockHash, p, constraint = Some(false)))
        }
      }
      .as(())

  private def drawJustificationDottedLines[G[_]: Applicative](
      g: Graphz[G],
      validators: Map[String, ValidatorsBlocks]
  ): G[Unit] =
    validators.values.toList
      .flatMap(_.values.toList.flatten)
      .traverse {
        case ValidatorBlock(blockHash, _, justifications) => {
          justifications
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

        }

      }
      .as(())

  private def nodesForTs(
      validatorId: String,
      ts: Long,
      blocks: ValidatorsBlocks,
      finalizedFringe: Set[String],
      base: String
  ): Map[String, Option[GraphStyle]] =
    blocks.get(ts) match {
      case Some(tsBlocks) =>
        (tsBlocks.map {
          case ValidatorBlock(blockHash, _, _) =>
            (blockHash -> styleFor(blockHash, finalizedFringe, base))
        }).toMap
      case None => Map(s"${ts.show}_$validatorId" -> Some(Invis))
    }

  private def validatorCluster[G[_]: Monad: GraphSerializer](
      id: String,
      blocks: ValidatorsBlocks,
      timeseries: List[Long],
      finalizedFringe: Set[String],
      base: String
  ): G[Graphz[G]] =
    for {
      g     <- Graphz.subgraph[G](s"cluster_$id", DiGraph, label = Some(id))
      nodes = timeseries.map(ts => nodesForTs(id, ts, blocks, finalizedFringe, base))
      _ <- nodes.traverse(
            ns =>
              ns.toList.traverse {
                case (name, style) => g.node(name, style = style, shape = Box)
              }
          )
      _ <- nodes.zip(nodes.drop(1)).traverse {
            case (n1s, n2s) =>
              n1s.keys.toList.traverse { n1 =>
                n2s.keys.toList.traverse { n2 =>
                  g.edge(n1, n2, style = Some(Invis))
                }

              }
          }
      _ <- g.close
    } yield g

  private def styleFor(blockHash: String, fringe: Set[String], base: String): Option[GraphStyle] =
    if (fringe.contains(blockHash) || blockHash == base) Some(Filled) else None

}
