package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{discriminated, provide, uint2}
import coop.rchain.rspace.internal.codecByteVector
import coop.rchain.shared.AttemptOps._
import History._
import cats.{Applicative, FlatMap}
import cats.effect.Sync
import cats.implicits._
sealed trait Trie
sealed trait NonEmptyTrie                                   extends Trie
case object EmptyTrie                                       extends Trie
final case class Skip(affix: ByteVector, hash: TriePointer) extends NonEmptyTrie
final case class Node(hash: PointerBlockPointer)            extends NonEmptyTrie
final case class Leaf(hash: LeafPointer)                    extends NonEmptyTrie

final case class TriePath(nodes: Vector[Trie], conflicting: Option[Trie], edges: KeyPath) {
  def append(affix: KeyPath, t: Trie): TriePath =
    this.copy(nodes = this.nodes :+ t, edges = this.edges ++ affix)
}

object TriePath {
  def empty: TriePath = TriePath(Vector(), None, Nil)
}

final case class History[F[_]: Sync](
    root: Blake2b256Hash,
    historyStore: HistoryStore[F],
    pointerBlockStore: PointerBlockStore[F]
) {

  private def storePointerBlock(pb: PointerBlock): F[Node] = {
    val hash = PointerBlock.hash(pb)
    pointerBlockStore.put(hash, pb).map(_ => Node(hash))
  }

  private def divideSkip(
      incomingTail: KeyPath,
      incomingIdx: Byte,
      incomingTrie: Trie,
      existingTail: KeyPath,
      existingIdx: Byte,
      existingPointer: Blake2b256Hash,
      prefixPath: KeyPath
  ): F[(List[Trie], Trie)] =
    for {
      maybeSkipToExisting <- skipOrFetch(existingTail, existingPointer, historyStore.get)
      maybeSkipToIncoming = skipOrValue(incomingTail, incomingTrie)
      pointerBlock = PointerBlock.create(
        (toInt(incomingIdx), maybeSkipToIncoming),
        (toInt(existingIdx), maybeSkipToExisting)
      )
      nodeToPB        <- storePointerBlock(pointerBlock)
      maybeSkipToNode = skipOrValue(prefixPath, nodeToPB)
    } yield (
      incomingTrie :: maybeSkipToExisting :: maybeSkipToIncoming :: nodeToPB :: maybeSkipToNode :: Nil,
      maybeSkipToNode
    )

  // TODO consider an indexedseq
  private def rebalanceInsert(
      newLeaf: Trie,
      remainingPath: KeyPath,
      partialTrie: TriePath
  ): F[List[Trie]] =
    partialTrie match {
      case TriePath(Vector(), None, Nil) => // kickstart a trie
        Applicative[F].pure(newLeaf :: skip(newLeaf, remainingPath) :: Nil)

      case TriePath(Vector(Skip(a, _), Leaf(_)), None, common)
          if common == a.toSeq.toList => // update 1 element trie
        Applicative[F].pure(newLeaf :: skip(newLeaf, common) :: Nil)

      case TriePath(Vector(), Some(Skip(affix, existingPointer)), Nil) => // split skip at root
        val affixBytes                   = affix.toSeq.toList
        val prefixPath                   = commonPrefix(affixBytes, remainingPath)
        val incomingHead :: incomingTail = remainingPath.drop(prefixPath.size)
        val existingHead :: existingTail = affixBytes.drop(prefixPath.size)
        for {
          dividedSkip <- divideSkip(
                          incomingTail,
                          incomingHead,
                          newLeaf,
                          existingTail,
                          existingHead,
                          existingPointer,
                          prefixPath
                        )
        } yield dividedSkip._1

      case TriePath(elems :+ Node(ptr), Some(Skip(affix, existingPointer)), common) => // split existing skip
        val incomingPath                 = remainingPath.drop(common.size)
        val affixBytes                   = affix.toSeq.toList
        val prefixPath                   = commonPrefix(incomingPath, affixBytes)
        val incomingHead :: incomingTail = incomingPath.drop(prefixPath.size)
        val existingHead :: existingTail = affixBytes.drop(prefixPath.size)

        for {
          dividedSkip <- divideSkip(
                          incomingTail,
                          incomingHead,
                          newLeaf,
                          existingTail,
                          existingHead,
                          existingPointer,
                          prefixPath
                        )
          (newElements, topTrie) = dividedSkip
          pointerBlock           <- pointerBlockStore.get(ptr)
          updatedPointerBlock    = pointerBlock.get.updated((toInt(common.last), topTrie) :: Nil)
          nodeToExistingPB       <- storePointerBlock(updatedPointerBlock)
          result                 <- rehash(common.init, elems, nodeToExistingPB, newElements)
        } yield result

      case TriePath(elems :+ Node(ptr), None, common) => // add to existing node
        for {
          pastPB <- pointerBlockStore.get(ptr).map(_.get)
          key    = remainingPath.drop(common.size)
          s      = skipOrValue(key, newLeaf)
          newPB  = pastPB.updated((toInt(common.last), s) :: Nil)
          node   <- storePointerBlock(newPB)
          result <- rehash(common.init, elems, node, newLeaf :: s :: Nil)
        } yield result

      case TriePath(elems :+ (_: Leaf), _, path) => // update value
        rehash(path, elems, newLeaf, Nil)
      case _ => Sync[F].raiseError(new RuntimeException("malformed trie"))
    }

  private def rehash(
      path: KeyPath,
      rest: Vector[Trie],
      lastSeen: Trie,
      acc: List[Trie]
  ): F[List[Trie]] = {
    type Params = (KeyPath, Vector[Trie], Trie, List[Trie])
    def go(params: Params): F[Either[Params, List[Trie]]] = params match {
      case (
          remainingPath: KeyPath,
          remainingPrefixNodes: Vector[Trie],
          nextLastSeen: Trie,
          currentAcc: List[Trie]
          ) =>
        remainingPrefixNodes match {
          case head :+ Node(ptr) =>
            for {
              pointerBlock <- pointerBlockStore
                               .get(ptr)
                               .map(_.get.updated((toInt(remainingPath.last), nextLastSeen) :: Nil))
              nodeToPB <- storePointerBlock(pointerBlock)
            } yield (remainingPath.init, head, nodeToPB, currentAcc :+ nextLastSeen).asLeft

          case head :+ Skip(affix, _) =>
            Applicative[F]
              .pure(
                (
                  remainingPath.dropRight(affix.size.toInt),
                  head,
                  Skip(affix, Trie.hash(nextLastSeen)),
                  currentAcc :+ nextLastSeen
                )
              )
              .map(_.asLeft)
          case Vector() => Applicative[F].pure(currentAcc :+ nextLastSeen).map(_.asRight)
        }
    }
    FlatMap[F].tailRecM((path, rest, lastSeen, acc))(go)
  }

  private def rebalanceDelete(fullPath: KeyPath, trieNodesOnPath: Vector[Trie]): F[List[Trie]] = {
    type Params = (KeyPath, Vector[Trie], Option[Trie])
    def go(params: Params): F[Either[Params, List[Trie]]] =
      params match {
        case (currentKey, init :+ Skip(affix, _), None) => // remove empty skip
          Applicative[F].pure((currentKey.dropRight(affix.size.toInt), init, None)).map(_.asLeft)

        case (currentKey, init :+ Skip(affix, _), Some(Skip(eaffix, ep))) => // merge skips
          rehash(currentKey.dropRight(affix.size.toInt), init, Skip(affix ++ eaffix, ep), Nil)
            .map(_.asRight)

        case (keyInit :+ keyLast, init :+ Node(p), None) => // handle pointer block
          pointerBlockStore.get(p).flatMap {
            case Some(pointerBlock) if pointerBlock.countNonEmpty == 2 =>
              // pointerBlock contains 1 more element, find it, wrap in skip and re-balance
              def getOther =
                pointerBlock
                  .updated((toInt(keyLast), EmptyTrie) :: Nil)
                  .toVector
                  .zipWithIndex
                  .filter(v => v._1 != EmptyTrie)
                  .head

              val (other, idx) = getOther
              Applicative[F].pure((keyInit, init, Some(pointTo(toByte(idx), other)))).map(_.asLeft)
            case Some(pb) => // pb contains 3+ elements, drop one, rehash
              val updated = pb.updated((toInt(keyLast), EmptyTrie) :: Nil)
              storePointerBlock(updated)
                .flatMap(node => rehash(keyInit, init, node, Nil).map(_.asRight))
            case None => Sync[F].raiseError(new RuntimeException("malformed trie"))
          }

        case (key, rest @ _ :+ (_: Node), Some(s: Skip)) =>
          rehash(key, rest, s, Nil).map(_.asRight)

        case (Nil, Vector(), Some(t)) => Applicative[F].pure(t :: Nil).map(_.asRight)

        case (Nil, Vector(), None) => Applicative[F].pure(EmptyTrie :: Nil).map(_.asRight)
      }
    FlatMap[F].tailRecM((fullPath, trieNodesOnPath, none[Trie]))(go)
  }

  def process(actions: List[HistoryAction]): F[History[F]] =
    // TODO this is an intermediate step to reproduce all the trie behavior
    // will evolve to a fold based implementation with partial tries
    actions
      .foldLeftM(this.root) {
        case (currentRoot, InsertAction(remainingPath, value)) =>
          for {
            traverseResult <- findPath(remainingPath, currentRoot)
            (_, triePath)  = traverseResult
            elements       <- rebalanceInsert(Leaf(value), remainingPath, triePath)
            _              <- historyStore.put(elements)
          } yield Trie.hash(elements.last)

        case (currentRoot, DeleteAction(remainingPath)) =>
          for {
            traverseResult <- findPath(remainingPath, currentRoot)
            (_, triePath)  = traverseResult
            elements <- triePath match {
                         case TriePath(elems :+ (_: Leaf), None, path) if remainingPath == path =>
                           rebalanceDelete(remainingPath, elems)
                         case _ =>
                           Applicative[F].pure[List[Trie]](Nil)
                       }
            _ <- historyStore.put(elements)
          } yield elements.lastOption.map(Trie.hash).getOrElse(currentRoot)
      }
      .map(newRoot => this.copy(root = newRoot))

  private[history] def findPath(key: KeyPath, start: Blake2b256Hash): F[(Trie, TriePath)] = {
    type Params = (Trie, KeyPath, TriePath)
    def traverse(params: Params): F[Either[Params, (Trie, TriePath)]] =
      params match {
        case (EmptyTrie, _, path) => Applicative[F].pure((EmptyTrie, path)).map(_.asRight)
        case (l: Leaf, Nil, path) => Applicative[F].pure((l, path append (Nil, l))).map(_.asRight)
        case (s @ Skip(affix, p), remainingPath, path) if remainingPath.startsWith(affix.toSeq) =>
          historyStore
            .get(p)
            .map(
              trie =>
                (
                  trie,
                  remainingPath.drop(affix.size.toInt),
                  path.append(affix.toSeq.toList, s)
                ).asLeft
            )
        case (s: Skip, _, path) => //not a prefix
          Applicative[F].pure((EmptyTrie, path.copy(conflicting = Some(s)))).map(_.asRight)
        case (node @ Node(ptr), h :: tail, path) =>
          pointerBlockStore.get(ptr).flatMap {
            case Some(pb) =>
              Applicative[F]
                .pure((pb.toVector(toInt(h)), tail, path.append(h :: Nil, node)))
                .map(_.asLeft)
            case None => Sync[F].raiseError(new RuntimeException("malformed trie"))
          }
        case _ => Sync[F].raiseError(new RuntimeException("malformed trie"))
      }
    for {
      node   <- historyStore.get(start)
      result <- FlatMap[F].tailRecM((node, key, TriePath.empty))(traverse)
    } yield result

  }

  private[history] def findPath(key: KeyPath): F[(Trie, TriePath)] =
    findPath(key, root)

  def close(): F[Unit] =
    for {
      _ <- historyStore.close()
      _ <- pointerBlockStore.close()
    } yield ()
}

object History {

  def skipOrFetch[F[_]: Applicative](
      path: KeyPath,
      pointer: Blake2b256Hash,
      fetch: Blake2b256Hash => F[Trie]
  ): F[Trie] =
    if (path.isEmpty) {
      fetch(pointer)
    } else {
      Applicative[F].pure(Skip(ByteVector(path), pointer))
    }

  def skipOrValue(path: KeyPath, t: Trie): Trie =
    if (path.isEmpty) {
      t
    } else {
      skip(t, path)
    }

  // this mapping is kept explicit on purpose
  @inline
  private[history] def toInt(b: Byte): Int =
    java.lang.Byte.toUnsignedInt(b)

  // this mapping is kept explicit on purpose
  @inline
  private[history] def toByte(i: Int): Byte =
    i.toByte

  def commonPrefix(l: KeyPath, r: KeyPath): KeyPath =
    (l.view, r.view).zipped.takeWhile { case (ll, rr) => ll == rr }.map(_._1).toSeq

  def skip(t: Trie, affix: KeyPath): Skip =
    Skip(ByteVector(affix), Trie.hash(t))

  def pointTo(idx: Byte, t: Trie): Trie =
    t match {
      case Skip(affix, p) => Skip(idx +: affix, p)
      case n: Node        => Skip(ByteVector(idx), Trie.hash(n))
      case l: Leaf        => Skip(ByteVector(idx), Trie.hash(l))
      case EmptyTrie      => EmptyTrie
    }

  type KeyPath = Seq[Byte]

  type PointerBlockPointer = Blake2b256Hash
  type TriePointer         = Blake2b256Hash
  type LeafPointer         = Blake2b256Hash

  object Trie {
    def hash(trie: Trie)(implicit codecTrie: Codec[Trie]): Blake2b256Hash =
      codecTrie
        .encode(trie)
        .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteVector))
        .get
  }

  private val codecNode  = Blake2b256Hash.codecBlake2b256Hash
  private val codecLeaf  = Blake2b256Hash.codecBlake2b256Hash
  private val codecSkip  = codecByteVector :: Blake2b256Hash.codecBlake2b256Hash
  private val codecEmpty = provide(EmptyTrie)

  implicit def codecTrie: Codec[Trie] =
    discriminated[Trie]
      .by(uint2)
      .subcaseP(0) {
        case n: Node => n
      }(codecNode.as[Node])
      .subcaseP(1) {
        case s: Skip => s
      }(codecSkip.as[Skip])
      .subcaseP(2) {
        case emptyTrie: EmptyTrie.type => emptyTrie
      }(codecEmpty)
      .subcaseP(3) {
        case l: Leaf => l
      }(codecLeaf.as[Leaf])

}
