package coop.rchain.rspace.state

import cats.Monad
import cats.syntax.all._
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.history._
import coop.rchain.state.{TrieExporter, TrieNode}

trait RSpaceExporter[F[_]] extends TrieExporter[F] {
  type KeyHash = Blake2b256Hash

  // Get current root
  def getRoot: F[KeyHash]
}

object RSpaceExporter {

  final case class Counter(skip: Int, take: Int)

  final case object EmptyHistoryException extends Exception

  type ReadParams[F[_]] = (
      // Keys (tries) to read
      Vector[TrieNode[Blake2b256Hash]],
      // Offset path
      Seq[(Blake2b256Hash, Option[Byte])],
      // Skip & take counter
      Counter,
      // Result tries as indexed collection
      Vector[TrieNode[Blake2b256Hash]]
  )

  def traverseTrie[F[_]: Monad](
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      getTrie: Blake2b256Hash => F[Trie]
  ): F[Vector[TrieNode[Blake2b256Hash]]] =
    startPath.headOption.fold(Vector.empty[TrieNode[Blake2b256Hash]].pure[F]) {
      case (root, _) =>
        val startParams: ReadParams[F] = (
          // First node is root / start path head
          Vector(TrieNode(root, isLeaf = false, Seq.empty)),
          startPath,
          Counter(skip, take),
          Vector.empty
        )
        startParams.tailRecM(traverseTrieRec[F](getTrie))
    }

  private def traverseTrieRec[F[_]: Monad](
      getTrie: Blake2b256Hash => F[Trie]
  )(params: ReadParams[F]): F[Either[ReadParams[F], Vector[TrieNode[Blake2b256Hash]]]] = {
    val (keys, path, Counter(skip, take), currentNodes) = params
    def getCounter(n: Int, items: Vector[TrieNode[Blake2b256Hash]]) =
      if (skip > 0) (Counter(skip + n, take), Vector.empty) else (Counter(skip, take + n), items)
    keys match {
      case _ if skip == 0 && take == 0 =>
        currentNodes.asRight[ReadParams[F]].pure[F]
      case Seq() =>
        currentNodes.asRight[ReadParams[F]].pure[F]
      case key +: keysRest =>
        for {
          trie <- getTrie(key.hash)
          // Find offset
          pathHead       = path.headOption
          isHeadPath     = pathHead.nonEmpty && pathHead.get._1 == key.hash
          isSelectedNode = isHeadPath && path.size == 1
          (offset, pathRest) = if (isHeadPath) {
            // Node is in the path, take offset and rest of the path
            (path.head._2, path.tail)
          } else {
            (none, path)
          }
          // Extract child nodes
          children                    = extractRefs(trie, key, offset)
          (childLeafs, childNotLeafs) = children.partition(_.isLeaf)
          (counter, collected) = if (isHeadPath && !isSelectedNode) {
            // Prefix path selected, continue traverse
            getCounter(0, Vector.empty)
          } else if (isSelectedNode) {
            // Selected node in the path, keep it and continue traverse
            getCounter(-1, Vector(key))
          } else if (path.nonEmpty) {
            // Path non empty, continue traverse
            getCounter(0, Vector.empty)
          } else {
            // Add child nodes to result history
            getCounter(-1, currentNodes ++ childLeafs :+ key)
          }
          // Tries left to process
          remainingKeys = childNotLeafs ++ keysRest
        } yield (remainingKeys, pathRest, counter, collected).asLeft
    }
  }

  private def extractRefs(
      trie: Trie,
      node: TrieNode[Blake2b256Hash],
      offset: Option[Byte]
  ): Vector[TrieNode[Blake2b256Hash]] =
    trie match {
      case EmptyTrie => Vector.empty
      // Collects key for cold store
      case Skip(_, LeafPointer(hash)) =>
        Vector(TrieNode(hash, isLeaf = true, node.path))
      // Collects child reference
      case Skip(_, NodePointer(hash)) =>
        Vector(TrieNode(hash, isLeaf = false, node.path))
      case PointerBlock(childs) =>
        // Collects hashes of child tries
        val itemsIndexed = childs.zipWithIndex
        // Filter by offset / repositioning for the starting path
        val items = offset.fold(itemsIndexed)(
          offsetVal => itemsIndexed.dropWhile { case (_, i) => (offsetVal & 0xff) > i }
        )
        items.flatMap {
          case (SkipPointer(hash), idx) =>
            Vector(TrieNode(hash, isLeaf = false, node.path :+ (node.hash, idx.toByte.some)))
          case (NodePointer(hash), idx) =>
            Vector(TrieNode(hash, isLeaf = false, node.path :+ (node.hash, idx.toByte.some)))
          case (LeafPointer(hash), idx) =>
            Vector(TrieNode(hash, isLeaf = false, node.path :+ (node.hash, idx.toByte.some)))
          case _ =>
            Vector.empty
        }
    }

  // Pretty printer helpers
  def pathPretty(path: (Blake2b256Hash, Option[Byte])) = {
    val (hash, idx) = path
    val idxStr      = idx.fold("--")(i => String.format("%02x", Integer.valueOf(i & 0xff)))
    s"$idxStr:${hash.bytes.toHex.take(8)}"
  }
}
