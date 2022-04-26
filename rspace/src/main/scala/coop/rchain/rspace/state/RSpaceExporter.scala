package coop.rchain.rspace.state

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree._
import coop.rchain.state.{TrieExporter, TrieNode}
import scodec.bits.ByteVector

trait RSpaceExporter[F[_]] extends TrieExporter[F] {
  type KeyHash = Blake2b256Hash

  // Get current root
  def getRoot: F[KeyHash]
}

object RSpaceExporter {

  final case class Counter(skip: Int, take: Int)

  final case object EmptyHistoryException extends Exception

  def traverseHistory[F[_]: Sync](
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      getFromHistory: ByteVector => F[Option[ByteVector]]
  ): F[Vector[TrieNode[Blake2b256Hash]]] = {

    val settings = ExportDataSettings(
      flagNodePrefixes = false,
      flagNodeKeys = true,
      flagNodeValues = false,
      flagLeafPrefixes = false,
      flagLeafValues = true
    )

    def createLastPrefix(prefixSeq: Seq[Blake2b256Hash]) =
      if (prefixSeq.isEmpty) None // Start from root
      else {
        // Max prefix length = 127 bytes.
        // Prefix coded 5 Blake256 elements (0 - size, 1..4 - value of prefix).
        assert(prefixSeq.size >= 5, "Invalid path during export.")
        val (sizePrefix: Int, seq) = (prefixSeq.head.bytes.head & 0xff, prefixSeq.tail)
        val prefix128: ByteVector  = seq.head.bytes ++ seq(1).bytes ++ seq(2).bytes ++ seq(3).bytes
        prefix128.take(sizePrefix.toLong).some
      }

    def constructNodes(leafKeys: Seq[Blake2b256Hash], nodeKeys: Seq[Blake2b256Hash]) = {
      val dataKeys    = leafKeys.map(TrieNode(_, isLeaf = true, Vector())).toVector
      val historyKeys = nodeKeys.map(TrieNode(_, isLeaf = false, Vector())).toVector

      dataKeys ++ historyKeys
    }

    def constructLastPath(lastPrefix: ByteVector, rootHash: Blake2b256Hash) = {
      val prefixSize = lastPrefix.size.toInt
      val sizeArray: Array[Byte] = lastPrefix.size.toByte +:
        (0 until 31).map(_ => 0x00.toByte).toArray
      val prefixZeros: Array[Byte] =
        (0 until 128 - prefixSize).map(_ => 0x00.toByte).toArray
      val prefix128Array = lastPrefix.toArray ++ prefixZeros
      val prefixBlake0   = Blake2b256Hash.fromByteArray(sizeArray)
      val prefixBlake1   = Blake2b256Hash.fromByteArray(prefix128Array.slice(0, 32))
      val prefixBlake2   = Blake2b256Hash.fromByteArray(prefix128Array.slice(32, 64))
      val prefixBlake3   = Blake2b256Hash.fromByteArray(prefix128Array.slice(64, 96))
      val prefixBlake4   = Blake2b256Hash.fromByteArray(prefix128Array.slice(96, 128))
      Vector(
        (rootHash, None),
        (prefixBlake0, None),
        (prefixBlake1, None),
        (prefixBlake2, None),
        (prefixBlake3, None),
        (prefixBlake4, None)
      )
    }

    def constructLastNode(
        lastHash: Blake2b256Hash,
        lastPath: Vector[(Blake2b256Hash, None.type)]
    ) =
      Vector(TrieNode(lastHash, isLeaf = false, lastPath))

    if (startPath.isEmpty) Vector[TrieNode[Blake2b256Hash]]().pure
    else
      for {
        pathSeq               <- Sync[F].delay(startPath.map(_._1))
        (rootHash, prefixSeq) = (pathSeq.head, pathSeq.tail)

        // TODO: Implemented a temporary solution with lastPrefix coding in 5 blake256 hashes.
        lastPrefix = createLastPrefix(prefixSeq)

        expRes <- sequentialExport(rootHash, lastPrefix, skip, take, getFromHistory, settings)

        (data, newLastPrefixOpt)                = expRes
        ExportData(_, nodeKeys, _, _, leafKeys) = data

        nodes = constructNodes(leafKeys, nodeKeys)

        nodesWithoutLast = nodes.dropRight(1)

        lastPath = newLastPrefixOpt.map(constructLastPath(_, rootHash)).getOrElse(Vector())

        lastHistoryNode = nodeKeys.lastOption
          .map(constructLastNode(_, lastPath))
          .getOrElse(Vector())

      } yield nodesWithoutLast ++ lastHistoryNode
  }
  // Pretty printer helpers
  def pathPretty(path: (Blake2b256Hash, Option[Byte])): String = {
    val (hash, idx) = path
    val idxStr      = idx.fold("--")(i => String.format("%02x", Integer.valueOf(i & 0xff)))
    s"$idxStr:${hash.bytes.toHex.take(8)}"
  }
}
