package coop.rchain.rspace.state.exporters

import java.nio.ByteBuffer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.state.RSpaceExporter
import coop.rchain.rspace.state.RSpaceExporter._
import coop.rchain.rspace.util.Lib.time

object RSpaceExporterItems {

  final case class StoreItems[KeyHash, Value](
      items: Seq[(KeyHash, Value)],
      lastPath: Seq[(KeyHash, Option[Byte])]
  )

  def getHistory[F[_]: Sync, Value](
      exporter: RSpaceExporter[F],
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      fromBuffer: ByteBuffer => Value
  ): F[StoreItems[Blake2b256Hash, Value]] =
    for {
      // Traverse and collect tuple space nodes
      nodes <- time("IMPL READ TRIE NODES")(exporter.getNodes(startPath, skip, take))

      // Get last entry / raise exception if empty (partial function)
      lastEntry <- nodes.lastOption.liftTo(EmptyHistoryException)

      // Load all history items / without leafs
      historyKeys = nodes.filterNot(_.isLeaf)
      keys        = historyKeys.distinct.map(_.hash)
      items       <- time("GET HISTORY NODES")(exporter.getHistoryItems(keys, fromBuffer))

      // TEMP: print exported tries
//      tries <- time("GET HISTORY NODES")(exporter.getHistoryItems(keys, { buf =>
//                Trie.codecTrie.decodeValue(BitVector(buf)).get
//              }))
//      _ = println(s"${tries.map(_._2).mkString("\n")}")

    } yield StoreItems(items, lastEntry.path :+ (lastEntry.hash, none))

  def getData[F[_]: Sync, Value](
      exporter: RSpaceExporter[F],
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      fromBuffer: ByteBuffer => Value
  ): F[StoreItems[Blake2b256Hash, Value]] =
    for {
      // Traverse and collect tuple space nodes
      nodes <- time("GET DATA NODES")(exporter.getNodes(startPath, skip, take))

      // Get last entry / raise exception if empty (partial function)
      lastEntry <- nodes.lastOption.liftTo(EmptyHistoryException)

      // Load all data items / without history
      dataKeys = nodes.filter(_.isLeaf)
      keys     = dataKeys.distinct.map(_.hash)
      items    <- time("DATA LOADED")(exporter.getDataItems(keys, fromBuffer))
    } yield StoreItems(items, lastEntry.path :+ (lastEntry.hash, none))
}
