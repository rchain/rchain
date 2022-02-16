package coop.rchain.finalization

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.{Applicative, Foldable, Monad}
import coop.rchain.casper.api.GraphzGenerator.{DagInfo, ValidatorsBlocks}
import coop.rchain.casper.api.ValidatorBlock
import coop.rchain.graphz._
import coop.rchain.casper.sim.Simulation._
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

class FinalizationSpec extends FlatSpec with Matchers {

  class NetworkRunner[F[_]: Sync] {

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

    def runInfinite(enableOutput: Boolean): F[Unit] = {
      val nets           = List(10).map(genNet(_, enableOutput))
      val startIteration = 1
      (nets, startIteration).tailRecM[F, Unit] {
        case (networks, iteration) =>
          println(s"Iteration $iteration")
          val newNetState = networks.traverse {
            case (net, name) =>
              randomTest(net, name, enableOutput).map { case (newNet, _) => (newNet, name) }
          }
          newNetState.map((_, iteration + 1).asLeft[Unit]) // Infinite loop
      }
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

  implicit val s = monix.execution.Scheduler.global

  val sut = new NetworkRunner[Task]()

  it should "run network with complete dag" in {
    val r        = sut.runDagComplete.runSyncUnsafe()
    val (end, _) = r
    val a = end.senders.toList.map(
      _.realFringes
        .map(_.toList.sortBy { case (k, _) => k.id }.map(_._2.id).toString())
    )
    println(a.mkString("\n"))

  }

  it should "run random network" ignore {
    val r        = sut.runRandom.runSyncUnsafe()
    val (end, _) = r.last
    val a = end.senders.toList.map(
      _.realFringes
        .map(_.toList.sortBy { case (k, _) => k.id }.map(_._2.id).toString())
    )
    println(a.mkString("\n"))

  }

  // This test is ignored by default to provide finite tests time execution
  // It makes sense to turn on this test only on the local machine for long-time finalization testing
  it should "run infinite test" ignore {
    sut.runInfinite(enableOutput = false).runSyncUnsafe()
  }

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
