package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.Resource.ResourceClass.ProduceResource
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.catscontrib.{Capture, IOUtil}
import coop.rchain.p2p.Network.{ErrorHandler, KeysStore}
import coop.rchain.p2p.effects._

import scala.annotation.tailrec
import scala.collection.mutable

trait Casper[F[_], A] {
  def addBlock(b: BlockMessage): F[Unit]
  def contains(b: BlockMessage): F[Boolean]
  def deploy(d: Deploy): F[Unit]
  def estimator: F[A]
  def proposeBlock: F[Option[BlockMessage]]
  def sendBlockWhenReady: F[Unit]
}

trait MultiParentCasper[F[_]] extends Casper[F, IndexedSeq[BlockMessage]]

object MultiParentCasper extends MultiParentCasperInstances {
  def apply[F[_]](implicit instance: MultiParentCasper[F]): MultiParentCasper[F] = instance
}

sealed abstract class MultiParentCasperInstances {
  def noOpsCasper[F[_]: Applicative]: MultiParentCasper[F] =
    new MultiParentCasper[F] {
      def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
      def contains(b: BlockMessage): F[Boolean] = false.pure[F]
      def deploy(r: Deploy): F[Unit]            = ().pure[F]
      def estimator: F[IndexedSeq[BlockMessage]] =
        Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
      def proposeBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
      def sendBlockWhenReady: F[Unit]           = ().pure[F]
    }

  def simpleCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler]
    : MultiParentCasper[F] = new MultiParentCasper[F] {
    def addBlock(b: BlockMessage): F[Unit]    = ().pure[F]
    def contains(b: BlockMessage): F[Boolean] = false.pure[F]
    def deploy(d: Deploy): F[Unit]            = ().pure[F]
    def estimator: F[IndexedSeq[BlockMessage]] =
      Applicative[F].pure[IndexedSeq[BlockMessage]](Vector(BlockMessage()))
    def proposeBlock: F[Option[BlockMessage]] = Applicative[F].pure[Option[BlockMessage]](None)
    def sendBlockWhenReady: F[Unit] =
      for {
        _           <- IOUtil.sleep[F](5000L)
        currentTime <- Time[F].currentMillis
        postState = RChainState().withResources(
          Seq(Resource(ProduceResource(Produce(currentTime.toInt)))))
        body   = Body().withPostState(postState)
        header = blockHeader(body, List.empty[ByteString])
        block  = blockProto(body, header, List.empty[Justification], ByteString.EMPTY)
        _      <- CommUtil.sendBlock[F](block)
      } yield ()
  }

  //TODO: figure out Casper key management for validators
  def hashSetCasper[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: Encryption: KeysStore: ErrorHandler](
      id: ByteString,
      genesis: BlockMessage = genesisBlock): MultiParentCasper[F] =
    //TODO: Get rid of all the mutable data structures and write proper FP code
    new MultiParentCasper[F] {
      type BlockHash = ByteString
      type Validator = ByteString

      private val _childMap: mutable.HashMap[BlockHash, mutable.HashSet[BlockHash]] =
        new mutable.HashMap[BlockHash, mutable.HashSet[BlockHash]]()
      private val _latestMessages: mutable.HashMap[Validator, BlockHash] =
        new mutable.HashMap[Validator, BlockHash]()
      private val blockLookup: mutable.HashMap[BlockHash, BlockMessage] =
        new mutable.HashMap[BlockHash, BlockMessage]()
      private val blockBuffer: mutable.HashSet[BlockMessage] =
        new mutable.HashSet[BlockMessage]()
      private val deployHist: mutable.HashSet[Deploy] = new mutable.HashSet[Deploy]()
      private val deployBuff: mutable.HashSet[Deploy] = new mutable.HashSet[Deploy]()
      private val decreasingOrder                     = Ordering[Int].reverse

      blockLookup += (genesis.blockHash -> genesis)

      def addBlock(b: BlockMessage): F[Unit] =
        for {
          success <- attemptAdd(b)
          _ <- if (success)
                Log[F].info(s"CASPER: added block ${hashString(b)}") *> reAttemptBuffer
              else Capture[F].capture { blockBuffer += b }
        } yield ()

      def contains(b: BlockMessage): F[Boolean] = Capture[F].capture {
        blockLookup.contains(b.blockHash)
      }

      def deploy(d: Deploy): F[Unit] =
        Capture[F].capture {
          deployBuff += d
          deployHist += d
        } *> Log[F].info(s"CASPER: Received deploy $d")

      def estimator: F[IndexedSeq[BlockMessage]] = {
        val fScores = Capture[F].capture { scoresMap }

        @tailrec
        def sortChildren[A <: (BlockHash) => Int](blks: IndexedSeq[BlockHash],
                                                  scores: A): IndexedSeq[BlockHash] = {
          val newBlks = blks
            .flatMap(b => {
              val empty                         = new mutable.HashSet[BlockHash]()
              val c: mutable.HashSet[BlockHash] = _childMap.getOrElse(b, empty)
              if (c.nonEmpty) {
                c.toIndexedSeq
              } else {
                IndexedSeq(b)
              }
            })
            .distinct
            .sortBy(scores)(decreasingOrder)
          if (newBlks == blks) {
            blks
          } else {
            sortChildren(newBlks, scores)
          }
        }

        fScores.map(sortChildren(IndexedSeq(genesis.blockHash), _).map(blockLookup))
      }

      def proposeBlock: F[Option[BlockMessage]] = {
        /*
         * Logic:
         *  -Score each of the blockDAG heads extracted from the block messages via GHOST
         *  -Let P = subset of heads such that P contains no conflicts and the total score is maximized
         *  -Let R = subset of deploy messages which are not included in DAG obtained by following blocks in P
         *  -If R is non-empty then create a new block with parents equal to P and (non-conflicting) txns obtained from R
         *  -Else if R is empty and |P| > 1 then create a block with parents equal to P and no transactions
         *  -Else None
         */

        val orderedHeads = estimator

        //TODO: define conflict model for blocks
        //(maybe wait until we are actually dealing with rholang terms)
        val p = orderedHeads.map(_.take(1)) //for now, just take top choice
        val r = p.map(blocks => {
          val remDeploys = deployHist.clone()
          bfTraverse(blocks)(parents(_).iterator.map(blockLookup)).foreach(b => {
            b.body.foreach(_.newCode.foreach(remDeploys -= _))
          })
          remDeploys
        })

        val proposal = p.flatMap(parents => {
          //TODO: Compute this properly
          val parentPoststate = parents.head.body.get.postState.get
          val justifications  = justificationProto(_latestMessages)
          r.map(requests => {
            if (requests.isEmpty) {
              if (parents.length > 1) {
                val body = Body()
                  .withPostState(parentPoststate)
                val header = blockHeader(body, parents.map(_.blockHash))
                val block  = blockProto(body, header, justifications, id)

                Some(block)
              } else {
                None
              }
            } else {
              //TODO: compute postState properly
              val newPostState = parentPoststate
                .withBlockNumber(parentPoststate.blockNumber + 1)
              //TODO: only pick non-conflicting deploys
              val deploys = requests.take(10).toSeq
              //TODO: include reductions
              val body = Body()
                .withPostState(newPostState)
                .withNewCode(deploys)
              val header = blockHeader(body, parents.map(_.blockHash))
              val block  = blockProto(body, header, justifications, id)

              Some(block)
            }
          })
        })

        proposal.flatMap {
          case mb @ Some(block) =>
            Log[F].info(s"CASPER: Proposed block ${hashString(block)}") *>
              addBlock(block) *>
              CommUtil.sendBlock[F](block) *>
              estimator
                .map(_.head)
                .flatMap(forkchoice => Log[F].info(s"New fork-choice is ${hashString(forkchoice)}")) *>
              Monad[F].pure[Option[BlockMessage]](mb)
          case _ => Monad[F].pure[Option[BlockMessage]](None)
        }
      }

      def sendBlockWhenReady: F[Unit] =
        Log[F]
          .info("CASPER: Checking if ready to propose a new block...")
          .flatMap(_ => {
            if (deployBuff.size < 10) {
              Log[F].info(
                s"CASPER: Not ready yet, only ${deployBuff.size} deploys accumulated, waiting...") *>
                IOUtil.sleep[F](60000L) //wait some time before checking again
            } else {
              val clearBuff = Capture[F].capture { deployBuff.clear() }

              proposeBlock *> clearBuff
            }
          })

      private def attemptAdd(b: BlockMessage): F[Boolean] = {
        val hash     = b.blockHash
        val bParents = parents(b)

        if (bParents.exists(p => !blockLookup.contains(p))) {
          //cannot add a block if not all parents are known
          false.pure[F]
        } else if (b.justifications.exists(j => !blockLookup.contains(j.latestBlockHash))) {
          //cannot add a block if not all justifications are known
          false.pure[F]
        } else {
          //TODO: check if block is an equivocation and update observed faults

          Capture[F].capture {
            blockLookup += (hash -> b) //add new block to total set

            //Assume that a non-equivocating validator must include
            //its own latest message in the justification. Therefore,
            //for a given validator the blocks are guaranteed to arrive in causal order.
            _latestMessages.update(b.sender, hash)

            //add current block as new child to each of its parents
            bParents.foreach(p => {
              val currChildren = _childMap.getOrElse(p, new mutable.HashSet[BlockHash]())
              currChildren += hash
            })
          } *> true.pure[F]
        }
      }

      private def reAttemptBuffer: F[Unit] = {
        val attempts   = blockBuffer.toList.traverse(b => attemptAdd(b).map(succ => b -> succ))
        val maybeAdded = attempts.map(_.find(_._2).map(_._1))

        maybeAdded.flatMap {
          case Some(added) =>
            Capture[F].capture { blockBuffer -= added } *> reAttemptBuffer

          case None => ().pure[F]
        }
      }

      private def hashParents(hash: BlockHash): Iterator[BlockHash] = {
        val b = blockLookup(hash)
        parents(b).iterator
      }

      private def scoresMap: mutable.HashMap[BlockHash, Int] = {
        val result = new mutable.HashMap[BlockHash, Int]() {
          final override def default(hash: BlockHash): Int = 0
        }

        _latestMessages.foreach {
          case (validator, lhash) =>
            //propagate scores for each validator along the dag they support
            bfTraverse[BlockHash](Some(lhash))(hashParents).foreach(hash => {
              val b         = blockLookup(hash)
              val currScore = result.getOrElse(hash, 0)

              val validatorWeight = mainParent(blockLookup, b)
                .map(weightMap(_).getOrElse(validator, 0))
                .getOrElse(weightMap(b).getOrElse(validator, 0)) //no parents means genesis -- use itself

              result.update(hash, currScore + validatorWeight)
            })

            //add scores to the blocks implicitly supported through
            //including a latest block as a "step parent"
            _childMap
              .get(lhash)
              .foreach(children => {
                children.foreach(cHash => {
                  val c = blockLookup(cHash)
                  if (parents(c).size > 1 && c.sender != validator) {
                    val currScore       = result(cHash)
                    val validatorWeight = weightMap(c).getOrElse(validator, 0)
                    result.update(cHash, currScore + validatorWeight)
                  }
                })
              })
        }

        result
      }
    }

  private def bfTraverse[A](start: Iterable[A])(neighbours: (A) => Iterator[A]): Iterator[A] =
    new Iterator[A] {
      private val visited    = new mutable.HashSet[A]()
      private val underlying = new mutable.Queue[A]()
      start.foreach(underlying.enqueue(_))

      @tailrec
      final override def hasNext: Boolean = underlying.headOption match {
        case None => false
        case Some(nxt) =>
          if (visited(nxt)) {
            underlying.dequeue() //remove already visited block
            hasNext              //try again to find existence of next block
          } else {
            true
          }
      }

      override def next(): A =
        if (hasNext) {
          val nxt = underlying.dequeue()
          visited.add(nxt)

          neighbours(nxt)
            .filterNot(a => visited(a)) //only add parents that have not already been visited
            .foreach(underlying.enqueue(_))

          nxt
        } else {
          Iterator.empty.next()
        }
    }
}
