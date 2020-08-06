package coop.rchain.rspace.state

import cats.Monad
import cats.syntax.all._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.history._
import coop.rchain.rspace.util.Lib
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
      Seq[TrieNode[Blake2b256Hash]],
      // Offset path
      Seq[(Blake2b256Hash, Option[Byte])],
      // Skip & take counter
      Counter,
      // Result tries as indexed collection
      Seq[TrieNode[Blake2b256Hash]],
      // Items traversed (not necessary)
      Int
  )

  def traverseTrie[F[_]: Monad](
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int,
      getTrie: Blake2b256Hash => F[Trie]
  ): F[Seq[TrieNode[Blake2b256Hash]]] =
    startPath.headOption.fold(Seq.empty[TrieNode[Blake2b256Hash]].pure[F]) {
      case (root, _) =>
        val startParams: ReadParams[F] = (
          // First node is root / start path head
          Seq(TrieNode(root, isLeaf = false, Nil)),
          startPath,
          Counter(skip, take),
          Seq.empty,
          0
        )
        Monad[F].tailRecM(startParams)(traverseTrieRec[F](getTrie))
    }

  private def traverseTrieRec[F[_]: Monad](
      getTrie: Blake2b256Hash => F[Trie]
  )(params: ReadParams[F]): F[Either[ReadParams[F], Seq[TrieNode[Blake2b256Hash]]]] = {
    val (keys, path, Counter(skip, take), currentNodes, iterations) = params
    def getCounter(n: Int, items: Seq[TrieNode[Blake2b256Hash]]) =
      if (skip > 0) (Counter(skip + n, take), Seq.empty) else (Counter(skip, take + n), items)
    keys match {
      case _ if skip == 0 && take == 0 =>
        println(s"ITERATIONS $iterations")
        currentNodes.asRight[ReadParams[F]].pure[F]
      case Nil =>
        println(s"ITERATIONS $iterations")
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
          children = extractRefs(trie, key, offset)
          (counter, collected) = if (isHeadPath && !isSelectedNode) {
            // Prefix path selected, continue traverse
            getCounter(0, Seq.empty)
          } else if (isSelectedNode) {
            // Selected node in the path, keep it and continue traverse
            getCounter(-1, Seq(key))
          } else if (path.nonEmpty) {
            // Path non empty, continue traverse
            getCounter(0, Seq.empty)
          } else {
            // Add child nodes to result history
            val childLeafs = children.filter(_.isLeaf)
            getCounter(-1, currentNodes ++ childLeafs :+ key)
          }
          // Tries left to process
          remainingKeys = children.filterNot(_.isLeaf) ++ keysRest
        } yield (remainingKeys, pathRest, counter, collected, iterations + 1).asLeft
    }
  }

  private def extractRefs(
      t: Trie,
      node: TrieNode[Blake2b256Hash],
      offset: Option[Byte]
  ): Seq[TrieNode[Blake2b256Hash]] =
    t match {
      case EmptyTrie => Seq.empty
      // Collects key for cold store
      case Skip(_, LeafPointer(hash)) =>
        Seq(TrieNode(hash, isLeaf = true, node.path))
      // Collects child reference
      case Skip(_, NodePointer(hash)) =>
        Seq(TrieNode(hash, isLeaf = false, node.path))
      case PointerBlock(childs) =>
        // Collects hashes of child tries
        val itemsIndexed = childs.zipWithIndex
        // Filter by offset / repositioning for the starting path
        val items = if (offset.nonEmpty) itemsIndexed.dropWhile {
          case (_, i) => (offset.get & 0xff) > i
        } else itemsIndexed
        items.flatMap {
          case (SkipPointer(hash), idx) =>
            Seq(TrieNode(hash, isLeaf = false, node.path :+ (node.hash, idx.toByte.some)))
          case (NodePointer(hash), idx) =>
            Seq(TrieNode(hash, isLeaf = false, node.path :+ (node.hash, idx.toByte.some)))
          case (LeafPointer(hash), idx) =>
            Seq(TrieNode(hash, isLeaf = false, node.path :+ (node.hash, idx.toByte.some)))
          case _ =>
            Seq.empty
        }
    }
}
