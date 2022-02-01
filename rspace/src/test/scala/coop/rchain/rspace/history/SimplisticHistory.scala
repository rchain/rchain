package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import cats.{Applicative, FlatMap}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.{commonPrefix, toByte, toInt, KeyPath}
import coop.rchain.rspace.history.HistoryMergingInstances.MalformedTrieError
import scodec.bits.ByteVector

import scala.Ordering.Implicits.seqDerivedOrdering

final case class SimplisticHistory[F[_]: Sync](
    root: Blake2b256Hash,
    historyStore: HistoryStore[F]
) extends HistoryWithFind[F] {
  override type HistoryF = HistoryWithFind[F]

  def skip(path: History.KeyPath, ptr: ValuePointer): (TriePointer, Option[Trie]) =
    if (path.isEmpty) {
      (ptr, None)
    } else {
      val s = Skip(ByteVector(path), ptr)
      (SkipPointer(Trie.hash(s)), Some(s))
    }

  def pointer(trie: Trie): TriePointer =
    trie match {
      case EmptyTrie        => EmptyPointer
      case pb: PointerBlock => NodePointer(pb.hash)
      case s: Skip          => SkipPointer(s.hash)
    }

  private def divideSkip(
      incomingTail: KeyPath,
      incomingIdx: Byte,
      incomingPointer: ValuePointer,
      existingTail: KeyPath,
      existingIdx: Byte,
      existingPointer: ValuePointer,
      prefixPath: KeyPath
  ): (List[Trie], TriePointer) = {
    val (newExistingPointer, maybeNewExistingSkip) = skip(existingTail, existingPointer)
    val (newIncomingPointer, maybeIncomingSkip)    = skip(incomingTail, incomingPointer)
    val pointerBlock = PointerBlock(
      (toInt(incomingIdx), newIncomingPointer),
      (toInt(existingIdx), newExistingPointer)
    )
    val pbPtr                            = NodePointer(pointerBlock.hash)
    val (topLevelPtr, maybeTopLevelSkip) = skip(prefixPath, pbPtr)
    val nodes = maybeTopLevelSkip match {
      case Some(value) => pointerBlock :: value :: Nil
      case None        => pointerBlock :: Nil
    }
    val res = (maybeNewExistingSkip.toList ++ maybeIncomingSkip.toList) ++ nodes
    (res, topLevelPtr)
  }

  // TODO consider an indexedseq
  private def rebalanceInsert(
      newLeaf: LeafPointer,
      remainingPath: KeyPath,
      partialTrie: TriePath
  ): F[List[Trie]] =
    partialTrie match {
      case TriePath(Vector(), None, Nil) => // kickstart a trie
        Applicative[F].pure(Skip(ByteVector(remainingPath), newLeaf) :: Nil)

      case TriePath(Vector(Skip(skippedPath, LeafPointer(_))), None, common)
          if common == skippedPath.toSeq.toList => // update 1 element trie
        Applicative[F].pure(Skip(ByteVector(common), newLeaf) :: Nil)

      case TriePath(Vector(), Some(Skip(affix, existingPointer)), Nil) => // split skip at root
        val affixBytes                   = affix.toSeq.toList
        val prefixPath                   = commonPrefix(affixBytes, remainingPath)
        val incomingHead :: incomingTail = remainingPath.drop(prefixPath.size)
        val existingHead :: existingTail = affixBytes.drop(prefixPath.size)
        val (dividedElems, _) = divideSkip(
          incomingTail,
          incomingHead,
          newLeaf,
          existingTail,
          existingHead,
          existingPointer,
          prefixPath
        )
        Applicative[F].pure(dividedElems)

      case TriePath(
          elems :+ (pointerBlock: PointerBlock),
          Some(Skip(affix, existingPointer)),
          common
          ) => // split existing skip
        val incomingPath                 = remainingPath.drop(common.size)
        val affixBytes                   = affix.toSeq.toList
        val prefixPath                   = commonPrefix(incomingPath, affixBytes)
        val incomingHead :: incomingTail = incomingPath.drop(prefixPath.size)
        val existingHead :: existingTail = affixBytes.drop(prefixPath.size)
        val (dividedElems, dividedTopPtr) = divideSkip(
          incomingTail,
          incomingHead,
          newLeaf,
          existingTail,
          existingHead,
          existingPointer,
          prefixPath
        )
        val updatedPointerBlock = pointerBlock.updated((toInt(common.last), dividedTopPtr) :: Nil)
        for {
          result <- rehash(common.init, elems, updatedPointerBlock, dividedElems)
        } yield result

      case TriePath(elems :+ (pastPb: PointerBlock), None, common) => // add to existing node
        val key          = remainingPath.drop(common.size)
        val (ptr, nodes) = skip(key, newLeaf)
        val updatedPb    = pastPb.updated((toInt(common.last), ptr) :: Nil)
        for {
          result <- rehash(common.init, elems, updatedPb, nodes.toList)
        } yield result

      case TriePath(elems :+ Skip(affix, LeafPointer(_)), _, path) => // update value
        rehash(path.dropRight(affix.size.toInt), elems, Skip(affix, newLeaf), Nil)
      case _ => Sync[F].raiseError(MalformedTrieError)
    }

  private def rehash(
      path: KeyPath,
      rest: Vector[Trie],
      lastSeen: Trie,
      acc: List[Trie]
  ): F[List[Trie]] = {
    type Params = (KeyPath, Vector[Trie], Trie, List[Trie])
    def go(params: Params): F[Either[Params, List[Trie]]] =
      params match {
        case (
            remainingPath: KeyPath,
            remainingPrefixNodes: Vector[Trie],
            nextLastSeen: Trie,
            currentAcc: List[Trie]
            ) =>
          (remainingPrefixNodes, nextLastSeen) match {
            case (head :+ (pointerBlock: PointerBlock), other) =>
              val ptr     = pointer(other)
              val updated = pointerBlock.updated((toInt(remainingPath.last), ptr) :: Nil)
              Applicative[F].pure(
                (remainingPath.init, head, updated, currentAcc :+ nextLastSeen).asLeft
              )

            case (head :+ Skip(affix, _), pb: PointerBlock) =>
              Applicative[F]
                .pure(
                  (
                    remainingPath.dropRight(affix.size.toInt),
                    head,
                    Skip(affix, NodePointer(pb.hash)),
                    currentAcc :+ nextLastSeen
                  )
                )
                .map(_.asLeft)

            case (Vector(), _) => Applicative[F].pure(currentAcc :+ nextLastSeen).map(_.asRight)
            case _             => Sync[F].raiseError(MalformedTrieError)

          }
      }
    FlatMap[F].tailRecM((path, rest, lastSeen, acc))(go)
  }

  private def rebalanceDelete(fullPath: KeyPath, trieNodesOnPath: Vector[Trie]): F[List[Trie]] = {
    type Params = (KeyPath, Vector[Trie], Option[Trie])
    def go(params: Params): F[Either[Params, List[Trie]]] =
      params match {
        case (currentKey, init :+ Skip(affix, _), None) => // remove empty skip
          Applicative[F]
            .pure((currentKey.dropRight(affix.size.toInt), init, None))
            .map(_.asLeft)

        case (currentKey, init :+ Skip(affix, _), Some(Skip(eaffix, ep))) => // merge skips
          rehash(currentKey.dropRight(affix.size.toInt), init, Skip(affix ++ eaffix, ep), Nil)
            .map(_.asRight)

        case (keyInit :+ keyLast, init :+ (pointerBlock: PointerBlock), None) => // handle pointer block
          pointerBlock match {
            case _ if pointerBlock.countNonEmpty == 2 =>
              // pointerBlock contains 1 more element, find it, wrap in skip and re-balance
              val (other, idx) = pointerBlock
                .updated((toInt(keyLast), EmptyPointer) :: Nil)
                .toVector
                .zipWithIndex
                .filter(v => v._1 != EmptyPointer)
                .head
              val r: F[Trie] = other match {
                case sp: SkipPointer =>
                  historyStore.get(sp.hash).flatMap {
                    case Skip(affix, ptr) => Applicative[F].pure(Skip(toByte(idx) +: affix, ptr))
                    case _                => Sync[F].raiseError[Trie](MalformedTrieError)
                  }
                case n: NodePointer =>
                  Applicative[F].pure(Skip(ByteVector(toByte(idx)), n))
                case l: LeafPointer =>
                  Applicative[F].pure(Skip(ByteVector(toByte(idx)), l))
                case _ =>
                  Sync[F].raiseError[Trie](MalformedTrieError)
              }
              r.map { nv =>
                (keyInit, init, Some(nv)).asLeft
              }
            case _ => // pb contains 3+ elements, drop one, rehash
              val updated = pointerBlock.updated((toInt(keyLast), EmptyPointer) :: Nil)
              rehash(keyInit, init, updated, Nil).map(_.asRight)
          }

        case (key, rest @ _ :+ (_: PointerBlock), Some(s: Skip)) =>
          rehash(key, rest, s, Nil).map(_.asRight)

        case (Nil, Vector(), Some(t)) =>
          Applicative[F].pure(t :: Nil).map(_.asRight) // collapse to one element in trie

        case (Nil, Vector(), None) =>
          Applicative[F].pure(EmptyTrie :: Nil).map(_.asRight) // collapse to empty trie
      }
    FlatMap[F].tailRecM((fullPath, trieNodesOnPath, none[Trie]))(go)
  }

  def process(actions: List[HistoryAction]): F[HistoryWithFind[F]] =
    // TODO this is an intermediate step to reproduce all the trie behavior
    // will evolve to a fold based implementation with partial tries
    actions
      .sortBy(_.key)
      .foldLeftM(this.root) {
        case (currentRoot, InsertAction(remainingPath, value)) =>
          for {
            traverseResult <- findPath(remainingPath, currentRoot)
            (_, triePath)  = traverseResult
            elements       <- rebalanceInsert(LeafPointer(value), remainingPath, triePath)
            _              <- historyStore.put(elements)
          } yield Trie.hash(elements.last)

        case (currentRoot, DeleteAction(remainingPath)) =>
          for {
            traverseResult <- findPath(remainingPath, currentRoot)
            (_, triePath)  = traverseResult
            elements <- triePath match {
                         case TriePath(elems, None, path) if remainingPath == path =>
                           rebalanceDelete(remainingPath, elems)
                         case _ =>
                           Applicative[F].pure[List[Trie]](Nil)
                       }
            _ <- historyStore.put(elements)
          } yield elements.lastOption.map(Trie.hash).getOrElse(currentRoot)
      }
      .map(newRoot => this.copy(root = newRoot))

  private[history] def findPath(
      key: KeyPath,
      start: Blake2b256Hash
  ): F[(TriePointer, TriePath)] = {
    type Params = (Trie, KeyPath, TriePath)
    def traverse(params: Params): F[Either[Params, (TriePointer, TriePath)]] =
      params match {
        case (EmptyTrie, _, path) => Applicative[F].pure((EmptyPointer, path)).map(_.asRight)
        case (s @ Skip(affix, p), remainingPath, path) if remainingPath.startsWith(affix.toSeq) =>
          p match {
            case _: LeafPointer if remainingPath == affix.toSeq =>
              Applicative[F].pure((p, path.append(affix.toSeq, s)).asRight)
            case _: LeafPointer =>
              Sync[F].raiseError(MalformedTrieError)
            case _: NodePointer =>
              historyStore
                .get(p.hash)
                .map(
                  trie =>
                    (
                      trie,
                      remainingPath.drop(affix.size.toInt),
                      path.append(affix.toSeq.toList, s)
                    ).asLeft
                )
          }
        case (s: Skip, _, path) => //not a prefix
          Applicative[F].pure((EmptyPointer, path.copy(conflicting = Some(s)))).map(_.asRight)
        case (pb: PointerBlock, h :: tail, path) =>
          pb.toVector(toInt(h)) match {
            case e: EmptyPointer.type =>
              Applicative[F].pure((e, path.append(h :: Nil, pb)).asRight)
            case n: NodePointer =>
              historyStore.get(n.hash).map(t => (t, tail, path.append(h :: Nil, pb)).asLeft)
            case s: SkipPointer =>
              historyStore.get(s.hash).map(t => (t, tail, path.append(h :: Nil, pb)).asLeft)
            case l: LeafPointer if tail.isEmpty =>
              Applicative[F].pure((l, path.append(h :: Nil, pb)).asRight)
            case _: LeafPointer => Sync[F].raiseError(MalformedTrieError)
          }
        case _ => Sync[F].raiseError(MalformedTrieError)
      }
    for {
      node   <- historyStore.get(start)
      result <- FlatMap[F].tailRecM((node, key, TriePath.empty))(traverse)
    } yield result

  }

  private[history] def findPath(key: KeyPath): F[(TriePointer, TriePath)] =
    findPath(key, root)

  def find(key: KeyPath): F[(TriePointer, Vector[Trie])] = findPath(key).map {
    case (trie, path) => (trie, path.nodes)
  }

  def read(key: ByteVector): F[Option[ByteVector]] =
    find(key.toArray.toList).flatMap {
      case (trie, _) =>
        trie match {
          case LeafPointer(dataHash) => dataHash.bytes.some.pure[F]
          case EmptyPointer          => Applicative[F].pure(none)
          case _ =>
            Sync[F].raiseError(new RuntimeException(s"unexpected data at key $key, data: $trie"))

        }
    }

  def reset(root: Blake2b256Hash): F[HistoryWithFind[F]] = Sync[F].delay(this.copy(root = root))

}

object SimplisticHistory {
  def noMerging[F[_]: Sync](
      root: Blake2b256Hash,
      historyStore: HistoryStore[F]
  ): HistoryWithFind[F] =
    new SimplisticHistory[F](root, historyStore)
}
