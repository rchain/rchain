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
      nodes <- time("TRAVERSE TRIE FOR HISTORY")(exporter.getNodes(startPath, skip, take))

      // Get last entry / raise exception if empty (partial function)
      lastEntry <- nodes.lastOption.liftTo(EmptyHistoryException)

      // Load all history items / without leafs
      historyKeys = nodes.filterNot(_.isLeaf)
      keys        = historyKeys.distinct.map(_.hash)
      items       <- time("GET HISTORY BY KEYS")(exporter.getHistoryItems(keys, fromBuffer))
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
      nodes <- time("TRAVERSE TRIE FOR DATA")(exporter.getNodes(startPath, skip, take))

      // Get last entry / raise exception if empty (partial function)
      lastEntry <- nodes.lastOption.liftTo(EmptyHistoryException)

      // Load all data items / without history
      dataKeys = nodes.filter(_.isLeaf)
      keys     = dataKeys.distinct.map(_.hash)
      items    <- time("GET DATA BY KEYS")(exporter.getDataItems(keys, fromBuffer))
    } yield StoreItems(items, lastEntry.path :+ (lastEntry.hash, none))

  def getHistoryAndData[F[_]: Sync, Value](
      exporter: RSpaceExporter[F],
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      fromBuffer: ByteBuffer => Value
  ): F[(StoreItems[Blake2b256Hash, Value], StoreItems[Blake2b256Hash, Value])] =
    for {
      // Traverse and collect tuple space nodes
      nodes <- time("TRAVERSE TRIE")(exporter.getNodes(startPath, skip, take))

      // Get last entry / raise exception if empty (partial function)
      lastEntry <- nodes.lastOption.liftTo(EmptyHistoryException)

      // Split by leaf nodes
      (leafs, nonLeafs) = nodes.partition(_.isLeaf)
      historyKeys       = nonLeafs.distinct.map(_.hash)
      dataKeys          = leafs.distinct.map(_.hash)

      // Load all history and data items
      historyItems <- time("GET HISTORY BY KEYS")(exporter.getHistoryItems(historyKeys, fromBuffer))
      dataItems    <- time("GET DATA BY KEYS")(exporter.getDataItems(dataKeys, fromBuffer))

      history = StoreItems(historyItems, lastEntry.path :+ (lastEntry.hash, none))
      data    = StoreItems(dataItems, lastEntry.path :+ (lastEntry.hash, none))

    } yield (history, data)
}
