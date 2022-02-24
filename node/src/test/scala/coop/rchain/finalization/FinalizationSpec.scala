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

import scala.util.Random

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

    // Regression test was derived from infinite test via scala code printing
    def runRegression = {
      val enableOutput = false
      val nets         = List(10).map(genNet(_, enableOutput))
      nets.traverse { case (net, name) => regressionTest(net, name, enableOutput) }
    }

    def runInfinite(enableOutput: Boolean, generateCode: Boolean): F[Unit] = {
      val nets           = List(10).map(genNet(_, enableOutput))
      val startIteration = 1
      (nets, startIteration).tailRecM[F, Unit] {
        case (networks, iteration) =>
          if (!generateCode) {
            println(s"Iteration $iteration")
          }
          val newNetworks = splitMerge(networks, generateCode)
          newNetworks.map((_, iteration + 1).asLeft[Unit]) // Infinite loop
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

    private def regressionTest(
        net: Network,
        name: String,
        enableOutput: Boolean
    ): F[(Network, Int)] =
      for {
        // Just to begin for-comprehension from flatMap
        net1_     <- runSections(net, List((1, .0f)), s"start-$name", enableOutput)
        (net1, _) = net1_

        (n1022813136, n1083092637) = net.split(0.5096376f)
        n_167755778_               <- runSections(n1022813136, List((1, 0.6574136f)), "n1022813136", enableOutput)
        (n_167755778, _)           = n_167755778_
        n_1407371484_ <- runSections(
                          n1083092637,
                          List((4, 0.78288853f)),
                          "n1083092637",
                          enableOutput
                        )
        (n_1407371484, _)          = n_1407371484_
        n1316169810                = n_167755778 >|< n_1407371484
        (n_2131838046, n171242518) = n1316169810.split(0.24446732f)
        n1709027370_ <- runSections(
                         n_2131838046,
                         List((1, 0.35403788f)),
                         "n_2131838046",
                         enableOutput
                       )
        (n1709027370, _)             = n1709027370_
        n_936033305_                 <- runSections(n171242518, List((8, 0.8514173f)), "n171242518", enableOutput)
        (n_936033305, _)             = n_936033305_
        n_801110001                  = n1709027370 >|< n_936033305
        (n_1753456045, n_1181790855) = n_801110001.split(0.68316025f)
        n_1885490992_ <- runSections(
                          n_1753456045,
                          List((12, 0.80235255f)),
                          "n_1753456045",
                          enableOutput
                        )
        (n_1885490992, _) = n_1885490992_
        n_1112360861_ <- runSections(
                          n_1181790855,
                          List((10, 0.23738176f)),
                          "n_1181790855",
                          enableOutput
                        )
        (n_1112360861, _)          = n_1112360861_
        (n1502918575, n_36996899)  = n_1112360861.split(0.46712136f)
        n2137437950_               <- runSections(n1502918575, List((7, 0.8242769f)), "n1502918575", enableOutput)
        (n2137437950, _)           = n2137437950_
        n1281978524_               <- runSections(n_36996899, List((8, 0.67792714f)), "n_36996899", enableOutput)
        (n1281978524, _)           = n1281978524_
        n200080004                 = n1281978524 >|< n2137437950
        n_911563424                = n_1885490992 >|< n200080004
        (n1009429810, n_984777102) = n_911563424.split(0.15929568f)
        n1507742654_ <- runSections(
                         n1009429810,
                         List((2, 0.85035944f)),
                         "n1009429810",
                         enableOutput
                       )
        (n1507742654, _) = n1507742654_
        n_1346367828_ <- runSections(
                          n_984777102,
                          List((3, 0.6530022f)),
                          "n_984777102",
                          enableOutput
                        )
        (n_1346367828, _)           = n_1346367828_
        (n_1870652816, n_884652529) = n1507742654.split(0.4179837f)
        n221340169_                 <- runSections(n_1870652816, List((6, 0.707146f)), "n_1870652816", enableOutput)
        (n221340169, _)             = n221340169_
        n1791511529_ <- runSections(
                         n_884652529,
                         List((4, 0.08678591f)),
                         "n_884652529",
                         enableOutput
                       )
        (n1791511529, _)          = n1791511529_
        n1316284692               = n1791511529 >|< n_1346367828
        (n_973792882, n123943753) = n1316284692.split(0.5909483f)
        n_950423999_ <- runSections(
                         n_973792882,
                         List((7, 0.12110078f)),
                         "n_973792882",
                         enableOutput
                       )
        (n_950423999, _)           = n_950423999_
        n830674740_                <- runSections(n123943753, List((4, 0.053336143f)), "n123943753", enableOutput)
        (n830674740, _)            = n830674740_
        n1773789275                = n830674740 >|< n_950423999
        (n_12835555, n_1845453989) = n1773789275.split(0.6123032f)
        n1552341417_               <- runSections(n_12835555, List((10, 0.7348425f)), "n_12835555", enableOutput)
        (n1552341417, _)           = n1552341417_
        n470165866_ <- runSections(
                        n_1845453989,
                        List((10, 0.54476756f)),
                        "n_1845453989",
                        enableOutput
                      )
        (n470165866, _)           = n470165866_
        n_1739841122              = n1552341417 >|< n470165866
        n_23410212                = n_1739841122 >|< n221340169
        (n716266525, n285850228)  = n_23410212.split(0.38010907f)
        n2118929613_              <- runSections(n716266525, List((3, 0.085235894f)), "n716266525", enableOutput)
        (n2118929613, _)          = n2118929613_
        n_328185564_              <- runSections(n285850228, List((10, 0.84769547f)), "n285850228", enableOutput)
        (n_328185564, _)          = n_328185564_
        n_1099161676              = n_328185564 >|< n2118929613
        (n_1858996816, n80197349) = n_1099161676.split(0.24985152f)
        n110964627_ <- runSections(
                        n_1858996816,
                        List((14, 0.44141734f)),
                        "n_1858996816",
                        enableOutput
                      )
        (n110964627, _)             = n110964627_
        n_1585794645_               <- runSections(n80197349, List((13, 0.16910058f)), "n80197349", enableOutput)
        (n_1585794645, _)           = n_1585794645_
        n1008581210                 = n_1585794645 >|< n110964627
        (n_422046842, n_1538497912) = n1008581210.split(0.11095542f)
        n_95554015_ <- runSections(
                        n_422046842,
                        List((15, 0.38963544f)),
                        "n_422046842",
                        enableOutput
                      )
        (n_95554015, _) = n_95554015_
        n1042570617_ <- runSections(
                         n_1538497912,
                         List((10, 0.23109722f)),
                         "n_1538497912",
                         enableOutput
                       )
        (n1042570617, _)          = n1042570617_
        n45761784                 = n1042570617 >|< n_95554015
        (n1464488337, n826889543) = n45761784.split(0.23212159f)
        n_168127302_ <- runSections(
                         n1464488337,
                         List((10, 0.8466194f)),
                         "n1464488337",
                         enableOutput
                       )
        (n_168127302, _)            = n_168127302_
        n_188358953_                <- runSections(n826889543, List((2, 0.8328306f)), "n826889543", enableOutput)
        (n_188358953, _)            = n_188358953_
        (n_1391966993, n1671848291) = n_168127302.split(0.25164324f)
        n1190750483_ <- runSections(
                         n_1391966993,
                         List((13, 0.32787257f)),
                         "n_1391966993",
                         enableOutput
                       )
        (n1190750483, _)           = n1190750483_
        n995028883_                <- runSections(n1671848291, List((8, 0.77417606f)), "n1671848291", enableOutput)
        (n995028883, _)            = n995028883_
        (n_1269818380, n335670858) = n_188358953.split(0.08835584f)
        r                          <- runSections(n_1269818380, List((8, 0.07683784f)), "n_1269818380", enableOutput)
      } yield r
  }

  implicit val s = monix.execution.Scheduler.global

  val sut = new NetworkRunner[Task]()

  it should "run network with complete dag" ignore {
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

  it should "run regression test" in {
    val r        = sut.runRegression.runSyncUnsafe()
    val (end, _) = r.last
    val a = end.senders.toList.map(
      _.realFringes
        .map(_.toList.sortBy { case (k, _) => k.id }.map(_._2.id).toString())
    )
    println(a.mkString("\n"))
  }

  // This test is ignored by default to provide finite tests time execution
  // It makes sense to turn on this test only on the local machine for long-time finalization testing
  it should "run infinite test" in {
    sut.runInfinite(enableOutput = false, generateCode = true).runSyncUnsafe()
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
