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

case class Acc[G[_]](timeseries: List[Long] = List.empty, graph: G[Graphz[G]])

object GraphzGenerator {

  private def init[G[_]: Monad: GraphSerializer](name: String): Acc[G] =
    Acc[G](
      graph = Graphz[G](
        name,
        DiGraph,
        rankdir = Some(BT),
        node = Map("width" -> "0", "height" -> "0", "margin" -> "0.03", "fontsize" -> "8")
      )
    )

  def generate[
      F[_]: Monad: Sync: MultiParentCasperRef: Log: SafetyOracle: BlockStore,
      G[_]: Monad: GraphSerializer
  ](topoSort: Vector[Vector[BlockHash]]): F[G[Graphz[G]]] =
    for {
      acc <- topoSort.foldM(init[G]("dag")) {
              case (acc, blockHashes) =>
                for {
                  blocks    <- blockHashes.traverse(ProtoUtil.unsafeGetBlock[F])
                  timeEntry = blocks.head.getBody.getState.blockNumber
                  maybeLvl0 = if (timeEntry != 1) None
                  else
                    Some(for {
                      g           <- Graphz.subgraph[G](s"lvl0", DiGraph, rank = Some(Same))
                      _           <- g.node("0")
                      genesis     = blocks.head.getHeader.parentsHashList.head
                      genesisHash = PrettyPrinter.buildString(genesis)
                      _ <- g.node(
                            name = genesisHash,
                            shape = Msquare
                          )
                      _ <- g.close
                    } yield g)

                  lvlGraph = for {
                    g <- Graphz.subgraph[G](s"lvl$timeEntry", DiGraph, rank = Some(Same))
                    _ <- g.node(timeEntry.toString)
                    _ <- blocks.traverse(
                          b => {
                            val blockHash       = PrettyPrinter.buildString(b.blockHash)
                            val blockSenderHash = PrettyPrinter.buildString(b.sender)
                            g.node(
                              name = blockHash,
                              shape = Record,
                              color = Some(hashColor(blockSenderHash)),
                              label = Some(s""""{$blockHash|$blockSenderHash}"""")
                            )
                          }
                        )
                    _ <- g.close
                  } yield g
                  graph = for {
                    g <- acc.graph
                    _ <- maybeLvl0.getOrElse(().pure[G])
                    _ <- g.subgraph(lvlGraph)
                    _ <- blocks.traverse(
                          b =>
                            b.getHeader.parentsHashList.toList
                              .map(PrettyPrinter.buildString)
                              .traverse { parentHash =>
                                g.edge(PrettyPrinter.buildString(b.blockHash) -> parentHash)
                              }
                        )
                  } yield g
                } yield {
                  val timeEntries = timeEntry :: maybeLvl0.map(kp(0L)).toList
                  acc.copy(
                    timeseries = timeEntries ++ acc.timeseries,
                    graph = graph
                  )
                }

            }
      result <- Sync[F].delay {

                 val times = acc.timeseries.sorted.map(_.toString)

                 val timeseries: G[Graphz[G]] = for {
                   g     <- Graphz.subgraph[G]("timeseries", DiGraph)
                   _     <- times.traverse(n => g.node(name = n, shape = PlainText))
                   edges = times.zip(times.drop(1))
                   _     <- edges.traverse(g.edge)
                   _     <- g.close
                 } yield g

                 for {
                   g <- acc.graph
                   _ <- g.subgraph(timeseries)
                   _ <- g.close
                 } yield g

               }
    } yield result

  private def hashColor(hash: String): String =
    s""""#${hash.substring(0, 6)}""""

}
