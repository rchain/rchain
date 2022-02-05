package coop.rchain.rspace.history

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Applicative, FlatMap, Parallel}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History._
import coop.rchain.rspace.serializers.ScodecSerialize.{
  codecPointerBlock,
  codecSkip,
  codecTrie,
  RichAttempt
}
import coop.rchain.shared.Base16
import coop.rchain.shared.syntax.sharedSyntaxFs2Stream
import scodec.bits.{BitVector, ByteVector}

import scala.Function.tupled
import scala.Ordering.Implicits.seqDerivedOrdering
import scala.collection.concurrent.TrieMap

object HistoryMergingInstances {

  type Index            = Byte
  type LastModification = (KeyPath, Trie)
  type SubtrieAtIndex   = (Index, KeyPath, NonEmptyTriePointer)

  def MalformedTrieError = new RuntimeException("malformed trie")

  val emptyRoot: Trie               = EmptyTrie
  private[this] def encodeEmptyRoot = codecTrie.encode(emptyRoot).getUnsafe.toByteVector
  val emptyRootHash: Blake2b256Hash = Blake2b256Hash.create(encodeEmptyRoot)

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

  final case class MergingHistory[F[_]: Parallel: Concurrent: Sync](
      root: Blake2b256Hash,
      historyStore: CachingHistoryStore[F]
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
      val pointerBlock = PointerBlock(
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

        case TriePath(Vector(s @ Skip(skippedPath, LeafPointer(_))), None, common)
            if common == skippedPath.toSeq.toList => // update 1 element trie
          historyStore.drop(s :: Nil) >>
            Applicative[F].pure((Skip(ByteVector(common), newLeaf): Trie) :: Nil)

        case TriePath(Vector(), Some(s @ Skip(affix, existingPointer)), Nil) => // split skip at root
          val (dividedElems, _) = divideSkip(
            remainingPath,
            affix,
            newLeaf,
            existingPointer
          )
          historyStore.drop(s :: Nil) >> Applicative[F].pure(dividedElems)

        case TriePath(
            elems :+ (pointerBlock: PointerBlock),
            Some(s @ Skip(affix, existingPointer)),
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
          historyStore.drop(s :: pointerBlock :: Nil) >>
            rehash(common.init, elems, updatedPointerBlock, dividedElems)

        case TriePath(elems :+ (pastPb: PointerBlock), None, common) => // add to existing node
          val key          = remainingPath.drop(common.size)
          val (ptr, nodes) = skip(key, newLeaf)
          val updatedPb    = pastPb.updated((toInt(common.last), ptr) :: Nil)
          historyStore.drop(pastPb :: Nil) >>
            rehash(common.init, elems, updatedPb, nodes.toList)

        case TriePath(elems :+ (s @ Skip(affix, LeafPointer(_))), _, path) => // update value
          historyStore.drop(s :: Nil) >>
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
                historyStore.drop(pointerBlock :: Nil) >>
                  Applicative[F].pure(
                    (remainingPath.init, head, updated: Trie, currentAcc :+ nextLastSeen)
                      .asLeft[List[Trie]]
                  )

              case (head :+ (s @ Skip(affix, _)), pb: PointerBlock) =>
                historyStore.drop(s :: Nil) >>
                  Applicative[F]
                    .pure(
                      (
                        remainingPath.dropRight(affix.size.toInt),
                        head,
                        Skip(affix, NodePointer(pb.hash)): Trie,
                        currentAcc :+ nextLastSeen
                      )
                    )
                    .map(_.asLeft[List[Trie]])

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
          case (currentKey, init :+ (s @ Skip(affix, _)), None) => // remove empty skip
            historyStore.drop(s :: Nil) >>
              Applicative[F]
                .pure((currentKey.dropRight(affix.size.toInt), init, none[Trie]))
                .map(_.asLeft[List[Trie]])

          case (currentKey, init :+ (l @ Skip(affix, _)), Some((r @ Skip(eaffix, ep)))) => // merge skips
            historyStore.drop(l :: r :: Nil) >>
              rehash(currentKey.dropRight(affix.size.toInt), init, Skip(affix ++ eaffix, ep), Nil)
                .map(_.asRight[Params])

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
                historyStore.drop(pointerBlock :: Nil) >>
                  r.map { nv =>
                    (keyInit, init, nv.some).asLeft[List[Trie]]
                  }
              case _ => // pb contains 3+ elements, drop one, rehash
                val updated = pointerBlock.updated((toInt(keyLast), EmptyPointer) :: Nil)
                rehash(keyInit, init, updated, Nil).map(_.asRight)
            }

          case (key, rest @ _ :+ (pb: PointerBlock), Some(s: Skip)) =>
            historyStore.drop(pb :: Nil) >>
              rehash(key, rest, s, Nil).map(_.asRight[Params])

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
      def isDiverging(path: KeyPath, affix: ByteVector): Boolean =
        (path.view, affix.toSeq.view).zipped.exists((l, r) => l != r)

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
          case (path, Skip(affix, _)) if isDiverging(path, affix) =>
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

    def process(actions: List[HistoryAction]): F[HistoryWithFind[F]] =
      for {
        _ <- Sync[F].ensure(actions.pure[F])(
              new RuntimeException("Cannot process duplicate actions on one key")
            )(hasNoDuplicates)
        unsortedPartitions = actions.groupBy(_.key.head).toList
        partitions         = unsortedPartitions.map(v => v.map(p => p.sortBy(_.key)))
        // TODO: make processing of subtries parallel again,
        //  sequential execution is now to prevent the bug with root hash mismatch.
        // https://rchain.atlassian.net/browse/RCHAIN-3940
        trieRoot <- historyStore.get(this.root)
        roots <- fs2.Stream
                  .emits(
                    partitions.map(p => fs2.Stream.eval(processSubtree(trieRoot)(p._1, p._2)))
                  )
                  .parJoinProcBounded
                  .compile
                  .toList
        modified         = roots.flatMap(tupled(extractSubtrieAtIndex))
        unmodified       <- extractUnmodifiedRootElements(partitions)
        all              = modified ++ unmodified
        results          <- constructRoot(all)
        (newRoot, tries) = results
        _                <- historyStore.put(newRoot :: tries)
        newRootHash      = Trie.hash(newRoot)
        _                <- historyStore.commit(newRootHash)
        _                <- historyStore.clear()
      } yield this.copy(root = newRootHash)

    private def hasNoDuplicates(actions: List[HistoryAction]) =
      actions.map(_.key).toSet.size == actions.size

    private[rspace] def processSubtree(
        start: Trie
    )(index: Index, actions: List[HistoryAction]): F[(Index, Trie)] =
      for {
        result <- actions
                   .foldLeftM((start, Option.empty[LastModification])) {
                     case (
                         (currentRoot, previousModificationOpt),
                         InsertAction(remainingPath, value)
                         ) =>
                       insert(currentRoot, previousModificationOpt, remainingPath, value)

                     case (
                         (currentRoot, previousModificationOpt),
                         DeleteAction(remainingPath)
                         ) =>
                       delete(currentRoot, previousModificationOpt, remainingPath)
                   }
        (root, _) = result
      } yield (index, root)

    private def insert(
        currentRoot: Trie,
        previousModificationOpt: Option[LastModification],
        remainingPath: KeyPath,
        value: Blake2b256Hash
    ): F[(Trie, Option[LastModification])] =
      for {
        _              <- commitPreviousModification(remainingPath, previousModificationOpt)
        traverseResult <- findPath(remainingPath, currentRoot)
        (_, triePath)  = traverseResult
        elements       <- rebalanceInsert(LeafPointer(value), remainingPath, triePath)
        _              <- historyStore.put(elements)
      } yield (elements.last, (remainingPath, elements.last).some)

    private def delete(
        currentRoot: Trie,
        previousModificationOpt: Option[LastModification],
        remainingPath: KeyPath
    ): F[(Trie, Option[LastModification])] =
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
        elements.lastOption.getOrElse(currentRoot),
        elements.lastOption.map(t => (remainingPath, t)).orElse(previousModificationOpt)
      )

    private def extractSubtrieAtIndex(
        index: Index,
        node: Trie
    ): Seq[SubtrieAtIndex] =
      node match {
        case EmptyTrie => Nil //produced empty root
        case Skip(prefix, ptr) if index == prefix.head =>
          (index, prefix.tail.toSeq, ptr) :: Nil //produced skip at current index
        case _: Skip => Nil
        //started with PointerBlock to 2 sub-tries, the one not under index is left
        case pb: PointerBlock => // produced multiple sub tries
          val idx = toInt(index)
          val t   = pb.toVector(idx)
          t match {
            case EmptyPointer             => Nil
            case ptr: NonEmptyTriePointer => (index, Seq.empty, ptr) :: Nil
          }
      }

    private def extractUnmodifiedRootElements(
        partitions: List[(Index, List[HistoryAction])]
    ): F[List[SubtrieAtIndex]] =
      for {
        oldRoot   <- historyStore.get(this.root)
        changed   = partitions.map(_._1).toSet.map(toInt)
        unchanged = cutModifiedIndices(changed)(oldRoot)
      } yield unchanged

    private def cutModifiedIndices(changed: Set[Int])(root: Trie): List[SubtrieAtIndex] =
      root match {
        case EmptyTrie                                               => Nil
        case Skip(prefix, _) if changed.contains(toInt(prefix.head)) => Nil
        case Skip(prefix, ptr)                                       => (prefix.head, prefix.tail.toSeq, ptr) :: Nil
        case pointerBlock: PointerBlock =>
          pointerBlock.toVector.toList.zipWithIndex.collect {
            case (ptr: NonEmptyTriePointer, i) if !changed.contains(i) =>
              (toByte(i), Seq.empty, ptr)
          }
      }

    private def constructRoot(elems: List[SubtrieAtIndex]): F[(Trie, List[Trie])] =
      elems match {
        case Nil =>
          (EmptyTrie: Trie, List.empty[Trie]).pure[F]
        case (b, rest, vp: ValuePointer) :: Nil =>
          (Skip(ByteVector(b) ++ ByteVector(rest), vp): Trie, List.empty[Trie]).pure[F]
        case (b, rest, sp: SkipPointer) :: Nil =>
          collapseToSkip(b, rest, sp)
        case elems: List[(Byte, Seq[Byte], TriePointer)] =>
          constructPointerBlock(elems)
      }

    private def collapseToSkip(
        index: Index,
        rest: KeyPath,
        skipPointer: SkipPointer
    ): F[(Trie, List[Trie])] =
      for {
        skip <- historyStore.get(skipPointer.hash)
        r <- skip match {
              case Skip(affix, ptr) =>
                val t: Trie = Skip(ByteVector(index) ++ ByteVector(rest) ++ affix, ptr)
                t.pure[F]
              case _ =>
                Sync[F].raiseError[Trie](MalformedTrieError)
            }
      } yield (r, Nil)

    private def constructPointerBlock(
        elems: List[SubtrieAtIndex]
    ): F[(Trie, List[Trie])] =
      elems
        .traverse(tupled(processRootLevelTrie))
        .map(_.foldLeft(List.empty[(Int, TriePointer)], List.empty[Trie]) {
          case ((indexes, tries), (index, mods)) =>
            (index :: indexes, tries ++ mods)
        })
        .map(r => (PointerBlock.empty.updated(r._1): Trie, r._2))

    private def processRootLevelTrie(
        index: Index,
        affix: KeyPath,
        triePointer: NonEmptyTriePointer
    ): F[((Int, TriePointer), List[Trie])] =
      (affix, triePointer) match {
        case (Seq(), ptr)             => ((toInt(index), ptr: TriePointer), List.empty[Trie]).pure[F]
        case (rest, vp: ValuePointer) => wrapSkip(index, ByteVector(rest), vp)
        case (rest, vp: SkipPointer) =>
          for {
            skip <- historyStore.get(vp.hash)
            r <- skip match {
                  case Skip(affix, ptr) =>
                    wrapSkip(index, ByteVector(rest) ++ affix, ptr)
                  case _ =>
                    Sync[F].raiseError[((Int, TriePointer), List[Trie])](MalformedTrieError)
                }
          } yield r
      }

    private def wrapSkip(
        b: Byte,
        affix: ByteVector,
        vp: ValuePointer
    ): F[((Int, TriePointer), List[Trie])] = {
      val skip = Skip(affix, vp)
      val ptr  = pointer(skip)
      ((toInt(b), ptr), List[Trie](skip)).pure[F]
    }

    def find(key: KeyPath): F[(TriePointer, Vector[Trie])] = findPath(key).map {
      case (trie, path) => (trie, path.nodes)
    }

    def read(key: ByteVector): F[Option[ByteVector]] =
      find(key.toArray.toList).flatMap {
        case (trie, _) =>
          trie match {
            case LeafPointer(dataHash) => dataHash.bytes.some.pure[F]
            case EmptyPointer          => Applicative[F].pure(None)
            case _ =>
              Sync[F].raiseError(new RuntimeException(s"unexpected data at key $key, data: $trie"))

          }
      }

    private[history] def findPath(key: KeyPath): F[(TriePointer, TriePath)] =
      historyStore.get(root) >>= (findPath(key, _))

    private[history] def findPath(
        key: KeyPath,
        start: Trie
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
      (start, key, TriePath.empty).tailRecM(traverse)
    }

    override def reset(root: Blake2b256Hash): F[HistoryWithFind[F]] =
      Sync[F].delay(
        this.copy(root = root, historyStore = CachingHistoryStore(historyStore.historyStore))
      )

  }

  def merging[F[_]: Concurrent: Parallel](
      root: Blake2b256Hash,
      historyStore: HistoryStore[F]
  ): HistoryWithFind[F] =
    new MergingHistory[F](root, CachingHistoryStore(historyStore))

  final case class CachingHistoryStore[F[_]: Sync](historyStore: HistoryStore[F])
      extends HistoryStore[F] {
    private[rspace] val cache: TrieMap[Blake2b256Hash, Trie] = TrieMap.empty

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

    def clear(): F[Unit] = Sync[F].delay {
      cache.clear()
    }

    def drop(tries: List[Trie]): F[Unit] =
      Sync[F].delay {
        tries.foreach { t =>
          cache.remove(Trie.hash(t))
        }
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

/*
 * Type definitions for Merkle Trie implementation (History)
 */

sealed trait Trie

sealed trait NonEmptyTrie extends Trie

case object EmptyTrie extends Trie

final case class Skip(affix: ByteVector, ptr: ValuePointer) extends NonEmptyTrie {
  lazy val encoded: BitVector = codecSkip.encode(this).getUnsafe

  lazy val hash: Blake2b256Hash = Blake2b256Hash.create(encoded.toByteVector)

  override def toString: String =
    s"Skip(${hash}, ${affix.toHex}\n  ${ptr})"
}

final case class PointerBlock private (toVector: Vector[TriePointer]) extends NonEmptyTrie {
  def updated(tuples: List[(Int, TriePointer)]): PointerBlock =
    new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def countNonEmpty: Int = toVector.count(_ != EmptyPointer)

  lazy val encoded: BitVector = codecPointerBlock.encode(this).getUnsafe

  lazy val hash: Blake2b256Hash = Blake2b256Hash.create(encoded.toByteVector)

  override def toString: String = {
    // TODO: this is difficult to visualize, maybe XML representation would be useful?
    val pbs =
      toVector.zipWithIndex
        .filter { case (v, _) => v != EmptyPointer }
        .map { case (v, n) => s"<$v, ${Base16.encode(Array(n.toByte))}>" }
        .mkString(",\n  ")
    s"PB(${hash}\n  $pbs)"
  }
}

object Trie {

  /**
    * Creates hash of Merkle Trie
    *
    * TODO: Fix encoding to use codec for the whole [[Trie]] and not for specific inherited variant.
    */
  def hash(trie: Trie): Blake2b256Hash =
    trie match {
      case pb: PointerBlock  => pb.hash
      case s: Skip           => s.hash
      case _: EmptyTrie.type => HistoryMergingInstances.emptyRootHash
    }
}

object PointerBlock {
  val length = 256

  val empty: PointerBlock = new PointerBlock(Vector.fill(length)(EmptyPointer))

  def apply(first: (Int, TriePointer), second: (Int, TriePointer)): PointerBlock =
    PointerBlock.empty.updated(List(first, second))

  def unapply(arg: PointerBlock): Option[Vector[TriePointer]] = Option(arg.toVector)
}

/*
 * Trie pointer definitions
 */

sealed trait TriePointer

sealed trait NonEmptyTriePointer extends TriePointer {
  def hash: Blake2b256Hash
}

case object EmptyPointer extends TriePointer

sealed trait ValuePointer extends NonEmptyTriePointer

final case class LeafPointer(hash: Blake2b256Hash) extends ValuePointer

final case class SkipPointer(hash: Blake2b256Hash) extends NonEmptyTriePointer

final case class NodePointer(hash: Blake2b256Hash) extends ValuePointer

/*
 * Trie path used as a helper for traversal in History implementation
 */

final case class TriePath(nodes: Vector[Trie], conflicting: Option[Trie], edges: KeyPath) {
  def append(affix: KeyPath, t: Trie): TriePath =
    this.copy(nodes = this.nodes :+ t, edges = this.edges ++ affix)
}

object TriePath {
  def empty: TriePath = TriePath(Vector(), None, Nil)
}
