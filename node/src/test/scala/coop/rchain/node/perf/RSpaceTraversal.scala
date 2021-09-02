package coop.rchain.node.perf

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath
import coop.rchain.rspace.history.HistoryInstances.MalformedTrieError
import coop.rchain.rspace.history._
import scodec.bits.ByteVector

object RSpaceTraversal {

  final case class Counter(skip: Int, take: Int)

  final case object EmptyHistoryException extends Exception

  final case class TreeNode[KeyHash](
      hash: KeyHash,
      isLeaf: Boolean,
      path: Seq[(KeyHash, Option[Byte])],
      level: Int = 0,
      prefix: ByteVector,
      value: Option[Blake2b256Hash] = none
  )

  type ReadParams[F[_]] = (
      // Keys (tries) to read
      Vector[TreeNode[Blake2b256Hash]],
      // Offset path
      Seq[(Blake2b256Hash, Option[Byte])],
      // Skip & take counter
      Counter,
      // Result tries as indexed collection
      Vector[TreeNode[Blake2b256Hash]]
  )

  def getLeafs[F[_]: Monad](
      root: Blake2b256Hash,
      skip: Int,
      take: Int,
      getTrie: Blake2b256Hash => F[Trie]
  ): F[Vector[TreeNode[Blake2b256Hash]]] = {
    val startPath = Vector((root, none[Byte]))
    val startParams: ReadParams[F] = (
      // First node is root / start path head
      Vector(TreeNode(root, isLeaf = false, Seq.empty, prefix = ByteVector.empty)),
      startPath,
      Counter(skip, take),
      Vector.empty
    )
    startParams.tailRecM(traverseTrieRec[F](getTrie))
  }

  private def traverseTrieRec[F[_]: Monad](
      getTrie: Blake2b256Hash => F[Trie]
  )(params: ReadParams[F]): F[Either[ReadParams[F], Vector[TreeNode[Blake2b256Hash]]]] = {
    val (keys, path, Counter(skip, take), currentNodes) = params

    def getCounter(n: Int, items: Vector[TreeNode[Blake2b256Hash]]) =
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
//            getCounter(-1, Vector(key))
            getCounter(-1, Vector.empty)
          } else if (path.nonEmpty) {
            // Path non empty, continue traverse
            getCounter(0, Vector.empty)
          } else {
            // Add child nodes to result history
//            getCounter(-1, currentNodes ++ childLeafs :+ key)
            getCounter(-1, currentNodes ++ childLeafs)
          }
          // Tries left to process
          remainingKeys = childNotLeafs ++ keysRest
        } yield (remainingKeys, pathRest, counter, collected).asLeft
    }
  }

  private def extractRefs(
      trie: Trie,
      node: TreeNode[Blake2b256Hash],
      offset: Option[Byte]
  ): Vector[TreeNode[Blake2b256Hash]] =
    trie match {
      case EmptyTrie => Vector.empty
      // Collects key for cold store
      case Skip(suffix, LeafPointer(hash)) =>
        Vector(
          TreeNode(
            hash,
            isLeaf = true,
            node.path,
            level = node.prefix.size.toInt - 1,
            prefix = node.prefix ++ suffix,
            value = hash.some
          )
        )
      // Collects child reference
      case Skip(suffix, NodePointer(hash)) =>
        Vector(TreeNode(hash, isLeaf = false, node.path, prefix = node.prefix ++ suffix))
      case PointerBlock(childs) =>
        // Collects hashes of child tries
        val itemsIndexed = childs.zipWithIndex
        // Filter by offset / repositioning for the starting path
        val items = offset.fold(itemsIndexed)(
          offsetVal => itemsIndexed.dropWhile { case (_, i) => (offsetVal & 0xff) > i }
        )
        items.flatMap {
          case (SkipPointer(hash), idx) =>
            Vector(
              TreeNode(
                hash,
                isLeaf = false,
                node.path :+ (node.hash, idx.toByte.some),
//                prefix = node.prefix
                prefix = node.prefix :+ idx.toByte
              )
            )
          case (NodePointer(hash), idx) =>
            Vector(
              TreeNode(
                hash,
                isLeaf = false,
                node.path :+ (node.hash, idx.toByte.some),
//                prefix = node.prefix
                prefix = node.prefix :+ idx.toByte
              )
            )
          case (LeafPointer(hash), idx) =>
            throw new Exception(s"Leaf node $hash ($idx), prefix: ${node.prefix}")
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

  // Enumerate all keys
  type KVPairs = Vector[TriePointer]

  def getAll[F[_]: Sync](
      key: KeyPath,
      start: Blake2b256Hash,
      getTrie: Blake2b256Hash => F[Trie]
  ): F[KVPairs] = {
    type Params = (Trie, KeyPath, TriePath)
    def traverse(params: Params): F[Either[Params, KVPairs]] =
      params match {
        case (EmptyTrie, _, _) =>
          Vector[TriePointer](EmptyPointer).asRight[Params].pure[F]
        case (s @ Skip(affix, p), remainingPath, path) if remainingPath.startsWith(affix.toSeq) =>
          p match {
            case _: LeafPointer if remainingPath == affix.toSeq =>
//              Applicative[F].pure((p, path.append(affix.toSeq, s)).asRight)
              Vector(p: TriePointer).asRight[Params].pure[F]
            case _: LeafPointer =>
              Sync[F].raiseError(MalformedTrieError)
            case _: NodePointer =>
              getTrie(p.hash)
                .map(
                  trie =>
                    (
                      trie,
                      remainingPath.drop(affix.size.toInt),
                      path.append(affix.toSeq.toList, s)
                    ).asLeft
                )
          }
        case (_: Skip, _, _) => //not a prefix
//          Applicative[F].pure((EmptyPointer, path.copy(conflicting = Some(s)))).map(_.asRight)
          Vector[TriePointer](EmptyPointer).asRight[Params].pure[F]
        case (pb: PointerBlock, h :: tail, path) =>
          pb.toVector(java.lang.Byte.toUnsignedInt(h)) match {
            case e: EmptyPointer.type =>
//              Applicative[F].pure((e, path.append(h :: Nil, pb)).asRight)
              Vector[TriePointer](e).asRight[Params].pure[F]
            case n: NodePointer =>
              getTrie(n.hash).map(t => (t, tail, path.append(h :: Nil, pb)).asLeft)
            case s: SkipPointer =>
              getTrie(s.hash).map(t => (t, tail, path.append(h :: Nil, pb)).asLeft)
            case l: LeafPointer if tail.isEmpty =>
//              Applicative[F].pure((l, path.append(h :: Nil, pb)).asRight)
              Vector[TriePointer](l).asRight[Params].pure[F]
            case _: LeafPointer => Sync[F].raiseError(MalformedTrieError)
          }
        case _ => Sync[F].raiseError(MalformedTrieError)
      }
    for {
      node <- getTrie(start)
//      result <- FlatMap[F].tailRecM((node, key, TriePath.empty))(traverse)
      result <- (node, key, TriePath.empty).tailRecM(traverse)
    } yield result
  }
}
