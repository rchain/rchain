package coop.rchain.finalization

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.{Applicative, Foldable, Monad}
import coop.rchain.casper.api.GraphzGenerator.{DagInfo, ValidatorsBlocks}
import coop.rchain.casper.api.ValidatorBlock
import coop.rchain.casper.sim.Simulation.{initNetwork, runNetwork, Msg, Network}
import coop.rchain.graphz._

import scala.util.Random

class NetworkRunner[F[_]: Sync] {
  import NetworkRunner._

  def printDag(network: Network, name: String) = {
    // DAG toposort
    val msgs = network.senders.head.heightMap
    val topo = msgs.map { case (_, v) => v.toVector }.toVector

    for {
      ref <- Ref[F].of(Vector[String]())
      _ <- {
        implicit val ser: GraphSerializer[F] = new ListSerializerRef[F](ref)
        dagAsCluster[F](topo, "")
      }
      res <- ref.get
      _ = {
        val graphString = res.mkString

        val filePrefix = s"vdags/$name"

        // Save graph file
        //          Files.writeString(Path.of(s"$filePrefix.dot"), graphString)

        // Generate dot image
        import java.io.ByteArrayInputStream
        import scala.sys.process._

        val imgType  = "jpg"
        val fileName = s"$filePrefix.$imgType"
        println(s"Generating dot image: $fileName")

        val dotCmd = Seq("dot", s"-T$imgType", "-o", fileName)
        dotCmd #< new ByteArrayInputStream(graphString.getBytes) lineStream
      }
    } yield ()
  }

  def runSections(
      start: Network,
      roundSkip: List[(Int, Float)],
      prefix: String,
      enableOutput: Boolean
  ) = {
    val senderCount = start.senders.size
    roundSkip.foldM(start, 0) {
      case ((net, n), (height, skipP)) =>
        runNetwork(net, height, skipP).flatMap { res =>
          //            val name = s"$prefix-$n-$senderCount-$skipP"
          val name = s"$prefix-$n-$senderCount"

          printDag(res, name).whenA(enableOutput).as((res, n + 1))
        }
    }
  }

  def genNet(senders: Int, enableOutput: Boolean) =
    initNetwork(sendersCount = senders, stake = 1, enableOutput) -> s"net$senders"

  def runDagComplete = {
    val enableOutput = true
    val (net, _)     = genNet(6, enableOutput)
    runSections(net, List((6, .0f)), s"complete", enableOutput)
  }

  def runRandom = {
    val enableOutput = true
    val nets         = List(10).map(genNet(_, enableOutput))
    nets.traverse { case (net, name) => randomTest(net, name, enableOutput) }
  }

  // Regression test was derived from infinite test via scala code printing
  def runRegression(f: (NetworkRunner[F], Network, String, Boolean) => F[(Network, Int)]) = {
    val enableOutput = false
    val nets         = List(10).map(genNet(_, enableOutput))
    nets.traverse { case (net, name) => f(this, net, name, enableOutput) }
  }

  def runInfinite(enableOutput: Boolean, generateCode: Boolean): F[Unit] = {
    val nets           = List(10).map(genNet(_, enableOutput))
    val startIteration = 1
    (nets, startIteration).iterateForeverM {
      case (networks, iteration) =>
        if (!generateCode) {
          println(s"Iteration $iteration")
        }
        val newNetworks = splitMerge(networks, generateCode)
        newNetworks.map((_, iteration + 1)) // Infinite loop
    }
  }

  object Action extends Enumeration {
    val Split, Merge = Value
    def random: Value = Random.nextInt(2) match {
      case 0 => Split
      case 1 => Merge
    }
  }

  private def splitMerge(
      nets: List[(Network, String)],
      generateCode: Boolean
  ): F[List[(Network, String)]] = {
    def removeByIndexFrom[T](v: List[T], i: Int): List[T] = v.patch(i, List.empty, 1)
    def uniqueNameFor[T](t: T): String =
      "n" + (t, Random.nextInt).hashCode.toString.replace("-", "_")

    // Runs simulation for network with random number of rounds and skip percent
    def runRounds(namedNet: (Network, String)): F[(Network, String)] = {
      val rounds = Random.nextInt(15) + 1 // from 1 to 15 inclusive
      val skip   = Random.nextFloat
      for {
        r <- runSections(namedNet._1, List((rounds, skip)), namedNet._2, enableOutput = false)
      } yield {
        val newNet     = r._1
        val newNetName = uniqueNameFor(newNet)
        if (generateCode) {
          println(
            s"""${newNetName}_ <- runSections(${namedNet._2}, List(($rounds, ${skip}f)), "${namedNet._2}", enableOutput)""".stripMargin
          )
          println(s"($newNetName, _) = ${newNetName}_")
        }
        (newNet, newNetName)
      }
    }

    // Splits random network and runs random number of rounds for both parts
    def splitAndRun(nets: List[(Network, String)]): F[List[(Network, String)]] =
      for {
        index         <- Sync[F].delay(Random.nextInt(nets.size))
        splitPercent  = Random.nextFloat
        name          = nets(index)._2
        (left, right) = nets(index)._1.split(splitPercent)
        leftName      = uniqueNameFor(left)
        rightName     = uniqueNameFor(right)

        r <- if (left.senders.nonEmpty && right.senders.nonEmpty) {
              if (generateCode) {
                println(s"($leftName, $rightName) = $name.split(${splitPercent}f)")
              }
              for {
                leftNet  <- runRounds((left, leftName))
                rightNet <- runRounds((right, rightName))
              } yield {
                // Replace partition by index with leftNet and rightNet
                removeByIndexFrom(nets, index) :+ leftNet :+ rightNet
              }
            } else {
              nets.pure
            }
      } yield r

    // Merges two random networks into one
    def merge(nets: List[(Network, String)]): F[List[(Network, String)]] = Sync[F].delay {
      if (nets.length >= 2) {
        // Take 2 random indices, remove corresponding items and add merging of them
        val indexes             = Random.shuffle(nets.indices.toList).take(2)
        val (leftNet, rightNet) = (nets(indexes.head)._1, nets(indexes(1))._1)
        val leftName            = nets(indexes.head)._2
        val rightName           = nets(indexes(1))._2
        val mergedNets          = leftNet >|< rightNet
        val mergedName          = uniqueNameFor(mergedNets)

        if (generateCode) {
          println(s"$mergedName = $leftName >|< $rightName")
        }

        nets.zipWithIndex
          .filter { case (_, index) => !indexes.contains(index) }
          .map { case (namedNet, _) => namedNet } :+ (mergedNets, uniqueNameFor(mergedNets))
      } else {
        nets
      }
    }

    for {
      act <- Action.random match {
              case Action.Split => splitAndRun(nets)
              case Action.Merge => merge(nets)
            }
    } yield act
  }

  private def randomTest(net: Network, name: String, enableOutput: Boolean): F[(Network, Int)] =
    for {
      net1_     <- runSections(net, List((1, .0f)), s"start-$name", enableOutput)
      (net1, _) = net1_

      // Split network
      (fst, snd) = net1.split(.3f)

      fst1_     <- runSections(fst, List((5, .5f)), s"fst-$name", enableOutput)
      (fst1, _) = fst1_

      snd1_     <- runSections(snd, List((4, .4f)), s"snd-$name", enableOutput)
      (snd1, _) = snd1_

      // Merge networks
      net2 = fst1 >|< snd1

      (n11, n12)   = net2.split(.4f)
      (n111, n112) = n11.split(.5f)

      n111end_     <- runSections(n111, List((10, .5f)), s"n111-$name", enableOutput)
      (n111end, _) = n111end_
      n112end_     <- runSections(n112, List((4, .1f)), s"n112-$name", enableOutput)
      (n112end, _) = n112end_
      n12end_      <- runSections(n12, List((5, .4f)), s"n12-$name", enableOutput)
      (n12end, _)  = n12end_

      net3 = n112end >|< n12end
      net4 = net3 >|< n111end

      (n21, n22)   = net4.split(.3f)
      (n211, n212) = n21.split(.5f)

      n211end_     <- runSections(n211, List((13, .4f)), s"n211-$name", enableOutput)
      (n211end, _) = n211end_
      n212end_     <- runSections(n212, List((8, .4f)), s"n212-$name", enableOutput)
      (n212end, _) = n212end_
      n22end_      <- runSections(n22, List((5, .4f)), s"n22-$name", enableOutput)
      (n22end, _)  = n22end_

      net5 = n212end >|< n211end
      net6 = net5 >|< n22end

      r <- runSections(net6, List((5, .0f)), s"result-$name", enableOutput)
    } yield r
}

object NetworkRunner {

  // TODO: duplicated from Graphz project until Graphz will be more configurable

  def dagAsCluster[F[_]: Sync: GraphSerializer](
      topoSort: Vector[Vector[Msg]], // Block hash
      lastFinalizedBlockHash: String
  ): F[Graphz[F]] =
    for {
      acc <- topoSort.foldM(DagInfo.empty[F])(accumulateDagInfo[F](_, _))

      timeseries     = acc.timeseries.reverse
      firstTs        = timeseries.head
      validators     = acc.validators
      validatorsList = validators.toList.sortBy(_._1)
      g              <- initGraph[F]("dag")
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
                style = styleFor(ancestor, lastFinalizedBlockHash),
                shape = Box
              )
          )
      // create invisible edges from ancestors to first node in each cluster for proper alligment
      _ <- validatorsList.traverse {
            case (id, blocks) =>
              allAncestors.traverse(ancestor => {
                val nodes = nodesForTs(id, firstTs, blocks, lastFinalizedBlockHash).keys.toList
                nodes.traverse(node => g.edge(ancestor, node, style = Some(Invis)))
              })
          }
      // draw clusters per validator
      _ <- validatorsList.traverse {
            case (id, blocks) =>
              g.subgraph(
                validatorCluster(id, blocks, timeseries, lastFinalizedBlockHash)
              )
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

  private def accumulateDagInfo[F[_]: Sync](
      acc: DagInfo[F],
      blocks: Vector[Msg] // Block hash
  ): F[DagInfo[F]] = {
    val timeEntry = blocks.head.height.toLong
    val validators = blocks.map { b =>
      val blockHash       = b.id
      val blockSenderHash = b.sender.id.toString
      // TODO: Parent and justifications are the same
      val parents        = b.justifications.values.toList
      val justifications = b.justifications.values.toList
      val validatorBlocks =
        Map(timeEntry -> List(ValidatorBlock(blockHash, parents, justifications)))
      Map(blockSenderHash -> validatorBlocks)
    }
    acc
      .copy[F](
        timeseries = timeEntry :: acc.timeseries,
        validators = acc.validators |+| Foldable[Vector].fold(validators)
      )
      .pure[F]
  }

  private def validatorCluster[G[_]: Monad: GraphSerializer](
      id: String,
      blocks: ValidatorsBlocks,
      timeseries: List[Long],
      lastFinalizedBlockHash: String
  ): G[Graphz[G]] =
    for {
      g     <- Graphz.subgraph[G](s"cluster_$id", DiGraph, label = Some(id))
      nodes = timeseries.map(ts => nodesForTs(id, ts, blocks, lastFinalizedBlockHash))
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

  private def initGraph[G[_]: Monad: GraphSerializer](name: String): G[Graphz[G]] = {
    val fontSize = "10"
    Graphz[G](
      name,
      DiGraph,
      rankdir = Some(BT),
      splines = Some("false"),
      //      node = Map("width"     -> "0", "height" -> "0", "margin" -> ".03", "fontsize" -> "8"),
      graph = Map("fontsize" -> fontSize),
      node = Map("width"     -> "0", "height" -> "0", "margin" -> "\".1,.05\"", "fontsize" -> fontSize),
      edge = Map(
        "arrowsize" -> ".5",
        //        "arrowhead" -> "empty",
        "arrowhead" -> "open",
        "penwidth"  -> ".6"
        //        "color"     -> "\"#404040\""
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
        case ValidatorBlock(blockHash, parentsHashes, _) =>
          parentsHashes.traverse(p => g.edge(blockHash, p, constraint = Some(false)))
      }
      .as(())

  private def drawJustificationDottedLines[G[_]: Applicative](
      g: Graphz[G],
      validators: Map[String, ValidatorsBlocks]
  ): G[Unit] =
    validators.values.toList
      .flatMap(_.values.toList.flatten)
      .traverse {
        case ValidatorBlock(blockHash, _, justifications) =>
          justifications
            .traverse { j =>
              g.edge(
                blockHash,
                j,
                style = Some(Dotted),
                constraint = Some(false),
                arrowHead = Some(NoneArrow)
              )
            }
      }
      .as(())

  private def nodesForTs(
      validatorId: String,
      ts: Long,
      blocks: ValidatorsBlocks,
      lastFinalizedBlockHash: String
  ): Map[String, Option[GraphStyle]] =
    blocks.get(ts) match {
      case Some(tsBlocks) =>
        (tsBlocks.map {
          case ValidatorBlock(blockHash, _, _) =>
            (blockHash -> styleFor(blockHash, lastFinalizedBlockHash))
        }).toMap
      case None => Map(s"${ts.show}_$validatorId" -> Some(Invis))
    }

  private def styleFor(blockHash: String, lastFinalizedBlockHash: String): Option[GraphStyle] =
    if (blockHash == lastFinalizedBlockHash) Some(Filled) else None

}
