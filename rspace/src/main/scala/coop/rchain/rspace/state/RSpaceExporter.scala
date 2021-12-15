package coop.rchain.rspace.state

import cats.Monad
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
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

  def traverseTrie[F[_]: Monad](
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      getTrie: ByteVector => F[Option[ByteVector]]
  ): F[Vector[TrieNode[Blake2b256Hash]]] = {
    import coop.rchain.rspace.history.RadixTree._
    import scodec.bits.ByteVector
    val pathSeq                           = startPath.map(_._1)
    val (rootHash: ByteVector, prefixSeq) = (pathSeq.head.bytes, pathSeq.tail)
    //todo implemented a temporary solution with lastPrefix coding in 5 blake256 hashes
    val lastPrefix: Option[ByteVector] =
      if (prefixSeq.isEmpty) None //start from root
      else {
        //max prefix length = 127 bytes
        //prefix coded 5 Blake256 elements (0 - size, 1..4 - value of prefix)
        assert(prefixSeq.size >= 5, "Invalid path during export")
        val (sizePrefix: Int, seq) = (prefixSeq.head.bytes.head & 0xff, prefixSeq.tail)
        val prefix128: ByteVector  = seq.head.bytes ++ seq(1).bytes ++ seq(2).bytes ++ seq(3).bytes
        prefix128.take(sizePrefix.toLong).some
      }
    val settings =
      ExportDataSettings(expNP = false, expNK = true, expNV = false, expLP = false, expLV = true)
    for {
      expRes                                  <- sequentialExport(rootHash, lastPrefix, skip, take, getTrie, settings)
      (data, newLastPrefixOpt)                = expRes
      ExportData(_, nodeKeys, _, _, leafKeys) = data
      dataKeys = leafKeys.map { key =>
        val hash = Blake2b256Hash.fromByteArray(key.toArray)
        TrieNode(hash, isLeaf = true, Vector())
      }.toVector

      historyKeysWithoutLast = nodeKeys
        .dropRight(1)
        .map { key =>
          val hash = Blake2b256Hash.fromByteArray(key.toArray)
          TrieNode(hash, isLeaf = false, Vector())
        }
        .toVector

      lastHistoryKey = nodeKeys.lastOption match {
        case None => Vector()
        case Some(lastKey) =>
          val hash = Blake2b256Hash.fromByteArray(lastKey.toArray)
          val path = newLastPrefixOpt match {
            case None => Vector()
            case Some(prefix) =>
              val prefixSize = prefix.size.toInt
              val sizeArray: Array[Byte] = prefix.size.toByte +:
                (0 until 31).map(_ => 0x00.toByte).toArray
              val prefixZeros: Array[Byte] =
                (0 until 128 - prefixSize).map(_ => 0x00.toByte).toArray
              val prefix128Array = prefix.toArray ++ prefixZeros
              val blake0         = Blake2b256Hash.fromByteArray(sizeArray)
              val blake1         = Blake2b256Hash.fromByteArray(prefix128Array.slice(0, 32))
              val blake2         = Blake2b256Hash.fromByteArray(prefix128Array.slice(32, 64))
              val blake3         = Blake2b256Hash.fromByteArray(prefix128Array.slice(64, 96))
              val blake4         = Blake2b256Hash.fromByteArray(prefix128Array.slice(96, 128))
              Vector(
                (blake0, None),
                (blake1, None),
                (blake2, None),
                (blake3, None),
                (blake4, None)
              )
          }
          Vector(TrieNode(hash, isLeaf = false, path))
      }
      r = dataKeys ++ historyKeysWithoutLast ++ lastHistoryKey
    } yield r
  }
  // Pretty printer helpers
  def pathPretty(path: (Blake2b256Hash, Option[Byte])) = {
    val (hash, idx) = path
    val idxStr      = idx.fold("--")(i => String.format("%02x", Integer.valueOf(i & 0xff)))
    s"$idxStr:${hash.bytes.toHex.take(8)}"
  }
}
