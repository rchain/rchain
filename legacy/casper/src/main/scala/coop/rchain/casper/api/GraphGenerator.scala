package coop.rchain.casper.api

import cats.effect.Sync
import cats.syntax.all._
import cats.{Applicative, Monad}
import coop.rchain.graphz.{GraphSerializer, _}

import scala.collection.compat.immutable.LazyList

object GraphGenerator {
  final case class ValidatorBlock(
      id: String,
      sender: String,
      height: Long,
      justifications: List[String],
      fringe: Set[String]
  ) {
    override def hashCode(): Int = id.hashCode()
  }

  type ValidatorsBlocks = Map[Long, List[ValidatorBlock]]

  final case class DagInfo(
      validators: Map[String, ValidatorsBlocks],
      timeseries: Set[Long]
  )

  object DagInfo {
    def empty: DagInfo = DagInfo(validators = Map.empty, timeseries = Set.empty)
  }

  def dagAsCluster[F[_]: Sync](
      blocks: Vector[ValidatorBlock],
      ser: GraphSerializer[F]
  ): F[Graphz[F]] = {
    val acc            = blocks.foldLeft(DagInfo.empty)(accumulateDagInfo)
    val blockColorMap  = generateFringeColorMapping(blocks)
    val timeseries     = acc.timeseries.toList.sorted
    val lowestHeight   = timeseries.head
    val validators     = acc.validators
    val validatorsList = validators.toList.sortBy(_._1)
    for {
      g <- initGraph[F]("dag", ser)
      allAncestors = validatorsList
        .flatMap {
          case (_, blocks) =>
            blocks
              .get(lowestHeight)
              .map(_.flatMap(b => b.justifications))
              .getOrElse(List.empty[String])
        }
        .distinct
        .sorted

      // create invisible edges from ancestors to first node in each cluster for proper alignment
      _ <- validatorsList.traverse {
            case (valId, blocks) =>
              allAncestors.traverse { ancestor =>
                val nodes = nodesForHeight(lowestHeight, blocks, valId, blockColorMap).keys.toList
                nodes.traverse(node => g.edge(ancestor, node, style = Some(Invis)))
              }
          }

      // draw clusters per validator
      _ <- validatorsList.traverse {
            case (id, blocks) =>
              validatorCluster(id, blocks, timeseries, blockColorMap, ser)
          }

      // draw parent dependencies
      _ <- drawParentDependencies[F](g, validatorsList.map(_._2))

      // draw justification dotted lines
      showJustificationLines = true
      _ <- if (!showJustificationLines)
            drawJustificationDottedLines[F](g, validators)
          else
            ().pure[F]
      _ <- g.close
    } yield g
  }

  private def accumulateDagInfo(
      acc: DagInfo,
      block: ValidatorBlock
  ): DagInfo = {
    val blockHeight     = block.height
    val validatorBlocks = Map(block.sender -> Map(blockHeight -> List(block)))
    acc
      .copy(
        timeseries = acc.timeseries + blockHeight,
        validators = acc.validators |+| validatorBlocks
      )
  }

  private def validatorCluster[F[_]: Monad](
      validatorId: String,
      blocks: ValidatorsBlocks,
      timeseries: List[Long],
      blockColorMap: Map[String, (Option[String], Option[String])],
      ser: GraphSerializer[F]
  ): F[Graphz[F]] =
    for {
      g     <- Graphz.subgraph[F](s"cluster_$validatorId", DiGraph, ser, label = Some(validatorId))
      nodes = timeseries.map(ts => nodesForHeight(ts, blocks, validatorId, blockColorMap))
      _ <- nodes.traverse(
            ns =>
              ns.toList.traverse {
                case (name, (style, fill, border)) =>
                  // Node shape, style and color
                  val borderWidth     = border >> 2.some
                  val borderOrDefault = border orElse "#828282".some // or gray
                  g.node(
                    name,
                    shape = DoubleOctagon,
                    style = style,
                    color = fill,
                    border = borderOrDefault,
                    borderWidth = borderWidth
                  )
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

  private def initGraph[F[_]: Monad](name: String, ser: GraphSerializer[F]): F[Graphz[F]] = {
    val fontSize = "12"
    Graphz[F](
      name,
      DiGraph,
      ser,
      rankdir = Some(BT),
      splines = Some("false"),
      graph = Map("fontsize" -> fontSize),
      node = Map("width"     -> "0", "height" -> "0", "margin" -> "\".1,.05\"", "fontsize" -> fontSize),
      edge = Map(
        "arrowsize" -> ".5",
        // "arrowhead" -> "empty",
        "arrowhead" -> "open",
        "penwidth"  -> ".6"
        // "color"     -> "\"#404040\""
      )
    )
  }

  private def drawParentDependencies[G[_]: Applicative](
      g: Graphz[G],
      validators: List[ValidatorsBlocks]
  ): G[Unit] =
    validators
      .flatMap(_.values.toList.flatten)
      .traverse {
        case ValidatorBlock(id, _, _, justifications, _) =>
          justifications.traverse(p => g.edge(id, p, constraint = Some(false)))
      }
      .as(())

  private def drawJustificationDottedLines[G[_]: Applicative](
      g: Graphz[G],
      validators: Map[String, ValidatorsBlocks]
  ): G[Unit] =
    validators.values.toList
      .flatMap(_.values.toList.flatten)
      .traverse {
        case ValidatorBlock(id, _, _, justifications, _) =>
          justifications
            .traverse { j =>
              g.edge(
                id,
                j,
                style = Some(Dotted),
                constraint = Some(false),
                arrowHead = Some(NoneArrow)
              )
            }
      }
      .as(())

  /* Helper functions to generate block color mapping from fringes */

  private def generateFringeColorMapping(
      blocks: Vector[ValidatorBlock]
  ): Map[String, (Option[String], Option[String])] = {
    // Different color for each fringe
    val colors = LazyList(
      "#ff5e5e", // red
      "#b561ff", // purple
      "#00b803", // green
      "#636eff", // blue
      "#8dff87", // light green
      "#00d9ff", // light blue
      "#ffc400"  // yellow
    )
    val colorsInCycle = cycle(colors)

    val blockMap = blocks.foldLeft(Map[String, ValidatorBlock]()) {
      case (acc, b) =>
        acc + ((b.id, b))
    }

    // Collect all fringes, remove duplicates and sort
    val initFringeMap = Map[Set[ValidatorBlock], Set[ValidatorBlock]]()
    val fringes = blocks
      .foldLeft(initFringeMap) {
        case (acc, b) =>
          val fringe     = b.fringe.map(blockMap)
          val seenFringe = acc.getOrElse(fringe, Set())
          acc + ((fringe, seenFringe + b))
      }
      .filter(_._1.nonEmpty)
      .toList
      .sortBy(_._1.toList.map(_.height).maximumOption.getOrElse(-1L))

    // Zip fringes with colors
    val initColorMaps = (Map[String, String](), Map[String, String]())
    val (fillMap, borderMap) =
      fringes.zip(colorsInCycle).foldLeft(initColorMaps) {
        case ((cAcc, bAcc), ((cs, bs), color)) =>
          val newCs = cs.foldLeft(cAcc) { case (acc1, b) => acc1 + ((b.id, color)) }
          val newBs = bs.foldLeft(bAcc) { case (acc1, b) => acc1 + ((b.id, color)) }
          (newCs, newBs)
      }
    val keys = fillMap.keySet ++ borderMap.keySet
    keys.map(x => x -> (fillMap.get(x), borderMap.get(x))).toMap
  }

  private def cycle(xs: LazyList[String]): LazyList[String] = xs #::: cycle(xs)

  /* Helpers to generate node stype and color */

  // Creates map of node styles on block height
  private def nodesForHeight(
      height: Long,
      blocks: ValidatorsBlocks,
      validatorId: String,
      blockColorMap: Map[String, (Option[String], Option[String])]
  ): Map[String, (Option[GraphStyle], Option[String], Option[String])] =
    transformOnHeight(height, blocks)(styleForNode(_, blockColorMap))
      .getOrElse(heightNoBlocks(height, validatorId))

  // Node style for a block
  private def styleForNode(
      blockId: String,
      blockColorMap: Map[String, (Option[String], Option[String])]
  ): (Option[Filled.type], Option[String], Option[String]) =
    blockColorMap
      .get(blockId)
      .map {
        case (fill, border) =>
          val style = fill.map(_ => Filled)
          (style, fill, border)
      }
      .getOrElse((none, none, none))

  // Node style on height without blocks
  private def heightNoBlocks(
      ts: Long,
      validatorId: String
  ): Map[String, (Option[Invis.type], Option[String], Option[String])] =
    Map(s"${ts.show}_$validatorId" -> (Some(Invis), none, none))

  // Transforms blocks on height
  private def transformOnHeight[A](height: Long, blocks: ValidatorsBlocks)(
      f: String => A
  ): Option[Map[String, A]] =
    blocks.get(height).map(_.map(v => v.id -> f(v.id)).toMap)
}
