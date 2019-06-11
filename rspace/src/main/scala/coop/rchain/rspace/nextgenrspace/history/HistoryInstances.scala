package coop.rchain.rspace.nextgenrspace.history

import cats.{Applicative, FlatMap}
import cats.effect.Sync
import cats.implicits._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.nextgenrspace.history.History._
import scodec.bits.ByteVector
import Ordering.Implicits.seqDerivedOrdering

import scala.collection.concurrent.TrieMap

object HistoryInstances {

  def MalformedTrieError = new RuntimeException("malformed trie")

  final case class MergingHistory[F[_]: Sync](
      root: Blake2b256Hash,
      historyStore: CachingHistoryStore[F]
  ) extends History[F] {

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
        incomingPath: KeyPath,
        affix: ByteVector,
        incomingPointer: ValuePointer,
        existingPointer: ValuePointer
    ): (List[Trie], TriePointer) = {
      val affixBytes                                 = affix.toSeq.toList
      val prefixPath                                 = commonPrefix(incomingPath, affixBytes)
      val incomingIdx :: incomingTail                = incomingPath.drop(prefixPath.size)
      val existingIdx :: existingTail                = affixBytes.drop(prefixPath.size)
      val (newExistingPointer, maybeNewExistingSkip) = skip(existingTail, existingPointer)
      val (newIncomingPointer, maybeIncomingSkip)    = skip(incomingTail, incomingPointer)
      val pointerBlock = PointerBlock.create(
        (toInt(incomingIdx), newIncomingPointer),
        (toInt(existingIdx), newExistingPointer)
      )
      val pbPtr                            = NodePointer(pointerBlock.hash)
      val (topLevelPtr, maybeTopLevelSkip) = skip(prefixPath, pbPtr)
      val res                              = maybeNewExistingSkip ++ maybeIncomingSkip ++ Some(pointerBlock) ++ maybeTopLevelSkip
      (res.toList, topLevelPtr)
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
          val (dividedElems, _) = divideSkip(
            remainingPath,
            affix,
            newLeaf,
            existingPointer
          )
          Applicative[F].pure(dividedElems)

        case TriePath(
            elems :+ (pointerBlock: PointerBlock),
            Some(Skip(affix, existingPointer)),
            common
            ) => // split existing skip
          val incomingPath = remainingPath.drop(common.size)
          val (dividedElems, dividedTopPtr) = divideSkip(
            incomingPath,
            affix,
            newLeaf,
            existingPointer
          )
          val updatedPointerBlock = pointerBlock.updated((toInt(common.last), dividedTopPtr) :: Nil)
          rehash(common.init, elems, updatedPointerBlock, dividedElems)

        case TriePath(elems :+ (pastPb: PointerBlock), None, common) => // add to existing node
          val key          = remainingPath.drop(common.size)
          val (ptr, nodes) = skip(key, newLeaf)
          val updatedPb    = pastPb.updated((toInt(common.last), ptr) :: Nil)
          rehash(common.init, elems, updatedPb, nodes.toList)

        case TriePath(elems :+ Skip(affix, LeafPointer(_)), _, path) => // update value
          rehash(path.dropRight(affix.size.toInt), elems, Skip(affix, newLeaf), Nil)
        case _ => Sync[F].raiseError(MalformedTrieError)
      }

    private def rehash(
        path: KeyPath,
        rest: Vector[Trie],
        lastSeen: Trie,
        initialElements: List[Trie]
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
      FlatMap[F].tailRecM((path, rest, lastSeen, initialElements))(go)
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

    def findUncommonNode(
        currentPath: KeyPath,
        previousPath: KeyPath,
        previousRoot: Trie
    ): F[Option[Blake2b256Hash]] = {
      def commonPrefixWithLastDiverging(path: KeyPath, affix: ByteVector): Boolean =
        affix.toSeq.startsWith(path.dropRight(1))

      def extractPointerToPointerBlock(ptr: ValuePointer): Option[Blake2b256Hash] =
        ptr match {
          case _: LeafPointer    => none      // ignore leaf
          case NodePointer(hash) => hash.some // pick pointerBlock
        }

      def pathTerminatesInAffix(path: KeyPath, affix: KeyPath): Boolean =
        affix == path || affix.startsWith(path)

      type PathToTrie = (KeyPath, Trie)
      def go(params: PathToTrie): F[Either[(KeyPath, Trie), Option[Blake2b256Hash]]] =
        params match {
          // whatever lives beneath the skip can be persisted
          case (path, Skip(affix, ptr)) if pathTerminatesInAffix(path, affix.toSeq) =>
            Applicative[F].pure(extractPointerToPointerBlock(ptr).asRight)

          // traverse non-terminal skip
          case (path, Skip(affix, ptr)) if path.startsWith(affix.toSeq) =>
            ptr match {
              case _: LeafPointer => Sync[F].raiseError(MalformedTrieError)
              case NodePointer(hash) =>
                historyStore
                  .get(hash)
                  .map(v => (path.drop(affix.size.toInt), v).asLeft)
            }

          // the path does not exist but some other skip lives near it == a node was removed
          case (path, Skip(affix, _)) if commonPrefixWithLastDiverging(path, affix) =>
            Applicative[F].pure(None.asRight) // removed path

          case (b :: Nil, PointerBlock(pointers)) => // interpret terminal pointer block
            (pointers(toInt(b)) match {
              case SkipPointer(hash) => hash.some
              case NodePointer(hash) => hash.some
              case _: LeafPointer    => none[Blake2b256Hash] // ignore leaf
              case EmptyPointer      => none[Blake2b256Hash] // removed path
            }).asRight[PathToTrie].pure[F]

          case (b :: rest, PointerBlock(pointers)) => // traverse non-terminal pointer block
            pointers(toInt(b)) match {
              case SkipPointer(hash) => historyStore.get(hash).map(v => (rest, v).asLeft)
              case NodePointer(hash) => historyStore.get(hash).map(v => (rest, v).asLeft)
              case _: LeafPointer    => Sync[F].raiseError(MalformedTrieError)
              case EmptyPointer      => Applicative[F].pure(None.asRight) // removed path
            }

          case _ => Sync[F].raiseError(MalformedTrieError)
        }

      val common = commonPrefix(previousPath, currentPath)
      if (common == currentPath) Applicative[F].pure(none[Blake2b256Hash])
      else {
        val withFirstUncommon = previousPath.take(common.size + 1)
        Sync[F].tailRecM((withFirstUncommon, previousRoot))(go)
      }
    }

    def commitUncommonLeftSubtrie(
        currentPath: KeyPath,
        previousPath: KeyPath,
        previousRoot: Trie
    ): F[Unit] =
      for {
        hashOpt <- findUncommonNode(currentPath, previousPath, previousRoot)
        _ <- hashOpt match {
              case None       => Applicative[F].unit
              case Some(hash) => historyStore.commit(hash)
            }
      } yield ()

    def commitPreviousModification(
        currentPath: KeyPath,
        previousModificationOpt: Option[(KeyPath, Trie)]
    ): F[Unit] =
      previousModificationOpt match {
        case None => Applicative[F].unit
        case Some((previousPath, previousRoot)) =>
          commitUncommonLeftSubtrie(currentPath, previousPath, previousRoot)
      }

    def process(actions: List[HistoryAction]): F[History[F]] = {
      type LastModification = (KeyPath, Trie)
      val start: (Blake2b256Hash, Option[LastModification]) = (this.root, None)
      val sorted                                            = actions.sortBy(_.key)
      def insert(
          currentRoot: Blake2b256Hash,
          previousModificationOpt: Option[LastModification],
          remainingPath: KeyPath,
          value: Blake2b256Hash
      ): F[(Blake2b256Hash, Option[LastModification])] =
        for {
          _              <- commitPreviousModification(remainingPath, previousModificationOpt)
          traverseResult <- findPath(remainingPath, currentRoot)
          (_, triePath)  = traverseResult
          elements       <- rebalanceInsert(LeafPointer(value), remainingPath, triePath)
          _              <- historyStore.put(elements)
        } yield (Trie.hash(elements.last), (remainingPath, elements.last).some)

      def delete(
          currentRoot: Blake2b256Hash,
          previousModificationOpt: Option[LastModification],
          remainingPath: KeyPath
      ): F[(Blake2b256Hash, Option[LastModification])] =
        for {
          _              <- commitPreviousModification(remainingPath, previousModificationOpt)
          traverseResult <- findPath(remainingPath, currentRoot)
          (_, triePath)  = traverseResult
          elements <- triePath match {
                       case TriePath(elems, None, path) if remainingPath == path =>
                         rebalanceDelete(remainingPath, elems)
                       case _ =>
                         Applicative[F].pure[List[Trie]](Nil)
                     }
          _ <- historyStore.put(elements)
        } yield (
          elements.lastOption.map(Trie.hash).getOrElse(currentRoot),
          elements.lastOption.map(t => (remainingPath, t))
        )

      for {
        _ <- Sync[F].ifM((actions.map(_.key).toSet.size == actions.size).pure[F])(
              ifTrue = Applicative[F].unit,
              ifFalse = Sync[F].raiseError(
                new RuntimeException("Cannot process duplicate actions on one key")
              )
            )
        result <- sorted.foldLeftM(start) {
                   case (
                       (currentRoot, previousModificationOpt),
                       InsertAction(remainingPath, value)
                       ) =>
                     insert(currentRoot, previousModificationOpt, remainingPath, value)

                   case ((currentRoot, previousModificationOpt), DeleteAction(remainingPath)) =>
                     delete(currentRoot, previousModificationOpt, remainingPath)
                 }
        (newRoot, _) = result
        _            <- historyStore.commit(newRoot)
        _            <- historyStore.clear()
      } yield this.copy(root = newRoot)
    }

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

    override def close(): F[Unit] = historyStore.close()

    override def reset(root: Blake2b256Hash): History[F] = this.copy(root = root)

  }

  def merging[F[_]: Sync](root: Blake2b256Hash, historyStore: HistoryStore[F]): History[F] =
    new MergingHistory[F](root, CachingHistoryStore(historyStore))

  final case class CachingHistoryStore[F[_]: Sync](historyStore: HistoryStore[F])
      extends HistoryStore[F] {
    private[this] val cache: TrieMap[Blake2b256Hash, Trie] = TrieMap.empty

    override def put(tries: List[Trie]): F[Unit] =
      Sync[F].delay {
        tries.foreach { t =>
          cache.put(Trie.hash(t), t)
        }
      }

    override def get(key: Blake2b256Hash): F[Trie] =
      for {
        maybeValue <- Sync[F].delay { cache.get(key) }
        result <- maybeValue match {
                   case None    => historyStore.get(key)
                   case Some(v) => Applicative[F].pure(v)
                 }
      } yield result

    override def close(): F[Unit] = historyStore.close()

    def clear(): F[Unit] = Sync[F].delay {
      cache.clear()
    }

    def commit(key: Blake2b256Hash): F[Unit] = {
      def getValue(key: Blake2b256Hash): List[Trie] =
        cache.get(key).toList // if a key exists in cache - we want to process it

      def extractRefs(t: Trie): Seq[Blake2b256Hash] =
        t match {
          case pb: PointerBlock =>
            pb.toVector.toList.filter(_ != EmptyPointer).flatMap {
              case v: SkipPointer => v.hash :: Nil
              case v: NodePointer => v.hash :: Nil
              case _              => Nil
            }
          case Skip(_, LeafPointer(_))    => Nil
          case Skip(_, NodePointer(hash)) => hash :: Nil
          case EmptyTrie                  => Nil
        }

      def go(keys: List[Blake2b256Hash]): F[Either[List[Blake2b256Hash], Unit]] =
        if (keys.isEmpty) Sync[F].pure(().asRight)
        else {
          val head :: rest = keys
          for {
            ts   <- Sync[F].delay { getValue(head) }
            _    <- historyStore.put(ts)
            _    <- Sync[F].delay { cache.remove(head) }
            refs = ts.flatMap(extractRefs)
          } yield (refs ++ rest).asLeft
        }
      Sync[F].tailRecM(key :: Nil)(go)
    }

  }
}
