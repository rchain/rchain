package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import com.google.protobuf.ByteString.ByteIterator
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Registry.FixedRefs._
import coop.rchain.rholang.interpreter.storage.implicits._
import org.lightningj.util.ZBase32

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.{Seq => RootSeq}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Registry implements a radix tree for public lookup of one-sided bundles.
  * The radix tree is implemented as follows:
  * Nodes are maps.
  * The keys in the map are byte arrays of length [0, 1]
  * The values in the map are tuples:
  * (tag, edgeAdditional, data)
  * tag:
  * if tag is 0, it is a terminal edge (We absorb leaf nodes into their parent)
  * if tag is 1, data is a name where another map can be read.
  * edgeAdditional:
  * this is a bytestring that stores any additional edge labeling in the radix tree.
  * data:
  * if tag is 0, this is the stored data.
  * if tag is 1, this is a name where the process recurs.
  */
trait Registry[F[_]] {

  def testInstall(): F[Unit]

  def lookup(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def lookupCallback(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def insert(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def insertCallback(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def delete(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def deleteRootCallback(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def deleteCallback(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def publicLookup(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def publicRegisterRandom(args: RootSeq[ListChannelWithRandom]): F[Unit]

  def publicRegisterInsertCallback(args: RootSeq[ListChannelWithRandom]): F[Unit]
}

class RegistryImpl[F[_]](
    private val space: Runtime.RhoPureSpace[F],
    private val dispatcher: Runtime.RhoDispatch[F]
)(implicit F: Sync[F])
    extends Registry[F] {

  import Registry._
  private def commonPrefix(b1: ByteString, b2: ByteString): ByteString = {
    val prefixOut = ByteString.newOutput()
    @tailrec
    def loop(it1: ByteIterator, it2: ByteIterator): ByteString =
      if (!it1.hasNext || !it2.hasNext) {
        prefixOut.toByteString
      } else {
        val b: Byte = it1.nextByte
        if (it2.nextByte == b) {
          prefixOut.write(b.toInt)
          loop(it1, it2)
        } else {
          prefixOut.toByteString()
        }
      }
    loop(b1.iterator, b2.iterator)
  }

  private def safeUncons(b: ByteString): (ByteString, ByteString) = {
    val head = if (b.isEmpty()) b else b.substring(0, 1)
    val tail = if (b.isEmpty()) b else b.substring(1)
    (head, tail)
  }

  private val lookupPatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)), ChanVar(FreeVar(1))),
      freeCount = 2
    )
  )
  // Testing only
  private val lookupChannels  = List(Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](10))))))
  private val insertRef: Long = Runtime.BodyRefs.REG_INSERT
  private val insertPatterns = List(
    BindPattern(
      Seq(
        Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
        Quote(Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)),
        ChanVar(FreeVar(2))
      ),
      freeCount = 3
    )
  )
  // Testing only
  private val insertChannels = List(Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](12))))))
  private val deletePatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)), ChanVar(FreeVar(1))),
      freeCount = 2
    )
  )
  // Testing only
  private val deleteChannels = List(Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](14))))))

  private val publicLookupRef: Long = Runtime.BodyRefs.REG_PUBLIC_LOOKUP
  // Testing only
  private val publicLookupChannels = List(
    Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](17)))))
  )
  private val publicLookupPatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)), ChanVar(FreeVar(1))),
      freeCount = 2
    )
  )

  private val publicRegisterRandomRef: Long = Runtime.BodyRefs.REG_PUBLIC_REGISTER_RANDOM
  private val publicRegisterInsertCallbackRef: Long =
    Runtime.BodyRefs.REG_PUBLIC_REGISTER_INSERT_CALLBACK
  // Testing only
  private val publicRegisterRandomChannels = List(
    Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](18)))))
  )
  private val publicRegisterRandomPatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)), ChanVar(FreeVar(1))),
      freeCount = 2
    )
  )

  private val publicRegisterInsertCallbackPatterns = List(
    BindPattern(
      Seq(
        Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
        Quote(Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)),
        ChanVar(FreeVar(2))
      ),
      freeCount = 3
    ),
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true))),
      freeCount = 1
    )
  )

  def testInstall(): F[Unit] =
    for {
      _ <- space.install(
            lookupChannels,
            lookupPatterns,
            TaggedContinuation(ScalaBodyRef(lookupRef))
          )
      _ <- space.install(
            insertChannels,
            insertPatterns,
            TaggedContinuation(ScalaBodyRef(insertRef))
          )
      _ <- space.install(
            deleteChannels,
            deletePatterns,
            TaggedContinuation(ScalaBodyRef(deleteRef))
          )
      _ <- space.install(
            publicLookupChannels,
            publicLookupPatterns,
            TaggedContinuation(ScalaBodyRef(publicLookupRef))
          )
      _ <- space.install(
            publicRegisterRandomChannels,
            publicRegisterRandomPatterns,
            TaggedContinuation(ScalaBodyRef(publicRegisterRandomRef))
          )
    } yield Unit

  private val prefixRetReplacePattern = BindPattern(
    Seq(
      Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
      ChanVar(FreeVar(1)),
      ChanVar(FreeVar(2))
    ),
    freeCount = 3
  )
  private val prefixValueRetReplacePattern = BindPattern(
    Seq(
      Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
      Quote(Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)),
      ChanVar(FreeVar(2)),
      ChanVar(FreeVar(3))
    ),
    freeCount = 4
  )
  private val parentKeyDataReplacePattern = BindPattern(
    Seq(
      Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
      Quote(Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)),
      ChanVar(FreeVar(2))
    ),
    freeCount = 3
  )
  private val triePattern = BindPattern(
    Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true))),
    freeCount = 1
  )

  private def parByteArray(bs: ByteString): Par = GByteArray(bs)

  private def handleResult[E <: Throwable](
      resultF: F[Either[E, Option[(TaggedContinuation, Seq[ListChannelWithRandom])]]]
  ): F[Unit] =
    resultF.flatMap({
      case Right(Some((continuation, dataList))) => dispatcher.dispatch(continuation, dataList)
      case Right(None)                           => F.unit
      case Left(err)                             => F.raiseError(err)
    })

  private def singleSend(data: Channel, chan: Quote, rand: Blake2b512Random): F[Unit] =
    handleResult(space.produce(chan, ListChannelWithRandom(Seq(data), rand, None), false))

  private def succeed(result: Par, ret: Channel, rand: Blake2b512Random): F[Unit] =
    ret match {
      case Channel(q @ Quote(_)) => singleSend(Quote(result), q, rand)
      case _                     => F.unit
    }

  private def fail(ret: Channel, rand: Blake2b512Random): F[Unit] =
    ret match {
      case Channel(q @ Quote(_)) => singleSend(Quote(Par()), q, rand)
      case _                     => F.unit
    }

  private def replace(data: Channel, replaceChan: Channel, dataRand: Blake2b512Random): F[Unit] =
    replaceChan match {
      case Channel(q @ Quote(_)) => singleSend(data, q, dataRand)
      case _                     => F.unit
    }

  private def failAndReplace(
      data: Channel,
      replaceChan: Channel,
      retChan: Channel,
      dataRand: Blake2b512Random,
      failRand: Blake2b512Random
  ): F[Unit] =
    for {
      _ <- replace(data, replaceChan, dataRand)
      _ <- fail(retChan, failRand)
    } yield ()

  private def fetchDataLookup(
      dataSource: Quote,
      key: Channel,
      ret: Channel,
      rand: Blake2b512Random
  ): F[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              Quote(channel),
              ListChannelWithRandom(Seq(key, ret, Channel(dataSource)), rand, None),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Channel](Quote(channel), dataSource),
              Seq(prefixRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(lookupCallbackRef)),
              false
            )
          )
    } yield ()
  }

  private def fetchDataInsert(
      dataSource: Quote,
      key: Channel,
      value: Channel,
      ret: Channel,
      rand: Blake2b512Random
  ): F[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              Quote(channel),
              ListChannelWithRandom(Seq(key, value, ret, Channel(dataSource)), rand, None),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Channel](Quote(channel), dataSource),
              Seq(prefixValueRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(insertCallbackRef)),
              false
            )
          )
    } yield ()
  }

  private def fetchDataRootDelete(
      dataSource: Quote,
      key: Channel,
      ret: Channel,
      rand: Blake2b512Random
  ): F[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              Quote(channel),
              ListChannelWithRandom(Seq(key, ret, Channel(dataSource)), rand, None),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Channel](Quote(channel), dataSource),
              Seq(prefixRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(deleteRootCallbackRef)),
              false
            )
          )
    } yield ()
  }

  private def fetchDataDelete(
      dataSource: Quote,
      key: Channel,
      ret: Channel,
      rand: Blake2b512Random,
      parentKey: Channel,
      parentData: Channel,
      parentReplace: Channel,
      parentRand: Blake2b512Random
  ): F[Unit] = {
    val keyChannel: Par    = GPrivate(ByteString.copyFrom(rand.next()))
    val parentChannel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              Quote(keyChannel),
              ListChannelWithRandom(Seq(key, ret, Channel(dataSource)), rand, None),
              false
            )
          )
      _ <- handleResult(
            space.produce(
              Quote(parentChannel),
              ListChannelWithRandom(Seq(parentKey, parentData, parentReplace), parentRand, None),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Channel](Quote(keyChannel), Quote(parentChannel), dataSource),
              Seq(prefixRetReplacePattern, parentKeyDataReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(deleteCallbackRef)),
              false
            )
          )
    } yield ()
  }

  def lookup(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, ret), rand, cost)) =>
        try {
          val Channel(Quote(keyPar))    = key
          val Some(Expr(GByteArray(_))) = keyPar.singleExpr
          fetchDataLookup(Quote(registryRoot), key, ret, rand)
        } catch {
          case _: MatchError => fail(ret, rand)
        }
      case _ => F.unit
    }

  // A lookup result should result in 1 of 3 things
  // Result not there. Return Nil
  // Result there, return it.
  // Further lookup needed, recurse.

  def lookupCallback(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(
          ListChannelWithRandom(Seq(key, ret, replaceChan), callRand, callCost),
          ListChannelWithRandom(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        try {
          val Channel(Quote(keyPar))                   = key
          val Some(Expr(GByteArray(bs)))               = keyPar.singleExpr
          val (head, tail)                             = safeUncons(bs)
          val Channel(Quote(dataPar))                  = data
          val Some(Expr(EMapBody(parMap)))             = dataPar.singleExpr
          val Some(value)                              = parMap.ps.get(Expr(GByteArray(head)))
          val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = value.singleExpr()
          if (ps.length != 3) {
            localFail()
          } else {
            // The second tuple field should be a bytearray in both cases.
            val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
            ps(0).singleExpr() match {
              case Some(Expr(GInt(0))) =>
                if (tail == edgeAdditional)
                  replace(data, replaceChan, dataRand).flatMap(_ => succeed(ps(2), ret, callRand))
                else
                  localFail()
              case Some(Expr(GInt(1))) =>
                if (tail.startsWith(edgeAdditional)) {
                  val newKey = tail.substring(edgeAdditional.size)

                  replace(data, replaceChan, dataRand).flatMap(
                    _ =>
                      fetchDataLookup(
                        Quote(ps(2)),
                        Channel(Quote(parByteArray(newKey))),
                        ret,
                        callRand
                      )
                  )
                } else
                  localFail()
            }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def insert(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, value, ret), rand, cost)) =>
        try {
          val Channel(Quote(keyPar))    = key
          val Some(Expr(GByteArray(_))) = keyPar.singleExpr
          fetchDataInsert(Quote(registryRoot), key, value, ret, rand)
        } catch {
          case _: MatchError => fail(ret, rand)
        }
      case _ => F.unit
    }

  def insertCallback(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(
          ListChannelWithRandom(Seq(key, value, ret, replaceChan), callRand, callCost),
          ListChannelWithRandom(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        try {
          val Channel(Quote(keyPar))       = key
          val Some(Expr(GByteArray(bs)))   = keyPar.singleExpr
          val (head, tail)                 = safeUncons(bs)
          val Channel(Quote(dataPar))      = data
          val Some(Expr(EMapBody(parMap))) = dataPar.singleExpr
          val Channel(Quote(valuePar))     = value
          def insert() = {
            val tuple: Par  = ETuple(Seq(GInt(0), parByteArray(tail), valuePar))
            val newMap: Par = ParMap(SortedParMap(parMap.ps + (parByteArray(head) -> tuple)))
            replace(Channel(Quote(newMap)), replaceChan, dataRand)
              .flatMap(_ => succeed(valuePar, ret, callRand))
          }
          parMap.ps.get(parByteArray(head)) match {
            case None => insert()
            case Some(mapEntry) =>
              val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = mapEntry.singleExpr()
              if (ps.length != 3)
                localFail()
              else {
                // The second tuple field should be a bytearray in both cases.
                val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
                def split() = {
                  val outgoingEdgeStr: ByteString = commonPrefix(edgeAdditional, tail)
                  val outgoingEdge: Par           = parByteArray(outgoingEdgeStr)
                  val oldEdgeStr: ByteString      = edgeAdditional.substring(outgoingEdgeStr.size())
                  val newEdgeStr: ByteString      = tail.substring(outgoingEdgeStr.size())
                  val (oldEdgeHead, oldEdgeTail)  = safeUncons(oldEdgeStr)
                  val (newEdgeHead, newEdgeTail)  = safeUncons(newEdgeStr)
                  val newMap: ParMap = ParMap(
                    SortedParMap(
                      Seq[(Par, Par)](
                        parByteArray(oldEdgeHead) -> ETuple(
                          Seq(ps(0), parByteArray(oldEdgeTail), ps(2))
                        ),
                        parByteArray(newEdgeHead) -> ETuple(
                          Seq(GInt(0), parByteArray(newEdgeTail), valuePar)
                        )
                      )
                    )
                  )
                  val newName: Par      = GPrivate(ByteString.copyFrom(callRand.next()))
                  val updatedTuple: Par = ETuple(Seq(GInt(1), outgoingEdge, newName))
                  val updatedMap: Par = ParMap(
                    SortedParMap(parMap.ps + (parByteArray(head) -> updatedTuple))
                  )
                  for {
                    _ <- replace(Quote(updatedMap), replaceChan, dataRand)
                    _ <- replace(Quote(newMap), Quote(newName), callRand.splitByte(0))
                    _ <- succeed(valuePar, ret, callRand.splitByte(1))
                  } yield ()
                }
                ps(0).singleExpr() match {
                  case Some(Expr(GInt(0))) =>
                    // Replace key
                    if (tail == edgeAdditional) {
                      insert()
                    } else {
                      split()
                    }
                  case Some(Expr(GInt(1))) =>
                    // If we have a complete match, recurse.
                    if (tail.startsWith(edgeAdditional)) {
                      val newKey = tail.substring(edgeAdditional.size)

                      replace(data, replaceChan, dataRand).flatMap(
                        _ =>
                          fetchDataInsert(
                            Quote(ps(2)),
                            Channel(Quote(parByteArray(newKey))),
                            value,
                            ret,
                            callRand
                          )
                      )
                    } else {
                      split()
                    }
                }
              }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def delete(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, ret), rand, cost)) =>
        try {
          val Channel(Quote(keyPar))    = key
          val Some(Expr(GByteArray(_))) = keyPar.singleExpr
          fetchDataRootDelete(Quote(registryRoot), key, ret, rand)
        } catch {
          case _: MatchError => fail(ret, rand)
        }
      case _ => F.unit
    }

  def deleteRootCallback(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(
          ListChannelWithRandom(Seq(key, ret, replaceChan), callRand, callCost),
          ListChannelWithRandom(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        try {
          val Channel(Quote(keyPar))                   = key
          val Some(Expr(GByteArray(bs)))               = keyPar.singleExpr
          val (head, tail)                             = safeUncons(bs)
          val Channel(Quote(dataPar))                  = data
          val Some(Expr(EMapBody(parMap)))             = dataPar.singleExpr
          val Some(value)                              = parMap.ps.get(parByteArray(head))
          val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = value.singleExpr()
          if (ps.length != 3)
            localFail()
          else {
            // The second tuple field should be a bytearray in both cases.
            val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
            ps(0).singleExpr() match {
              case Some(Expr(GInt(0))) =>
                if (tail == edgeAdditional) {
                  val updatedMap: Par = ParMap(SortedParMap(parMap.ps - parByteArray(head)))
                  replace(Quote(updatedMap), replaceChan, dataRand)
                    .flatMap(_ => succeed(ps(2), ret, callRand))
                } else {
                  localFail()
                }
              case Some(Expr(GInt(1))) =>
                if (tail.startsWith(edgeAdditional)) {
                  val newKey = tail.substring(edgeAdditional.size)
                  fetchDataDelete(
                    Quote(ps(2)),
                    Channel(Quote(parByteArray(newKey))),
                    ret,
                    callRand,
                    Quote(parByteArray(head)),
                    data,
                    replaceChan,
                    dataRand
                  )
                } else {
                  localFail()
                }
            }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def deleteCallback(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(
          ListChannelWithRandom(Seq(key, ret, replaceChan), callRand, callCost),
          ListChannelWithRandom(Seq(parentKey, parentData, parentReplace), parentRand, parentCost),
          ListChannelWithRandom(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() =
          replace(parentData, parentReplace, parentRand).flatMap(
            _ => failAndReplace(data, replaceChan, ret, dataRand, callRand)
          )
        try {
          val Channel(Quote(keyPar))     = key
          val Some(Expr(GByteArray(bs))) = keyPar.singleExpr
          val (head, tail)               = safeUncons(bs)

          def mergeWithParent(lastKey: Par, lastEntry: Par): F[Unit] = {
            val Channel(Quote(parentKeyPar))                   = parentKey
            val Channel(Quote(parentDataPar))                  = parentData
            val Some(Expr(EMapBody(parMap)))                   = parentDataPar.singleExpr()
            val Some(parentEntry)                              = parMap.ps.get(parentKeyPar)
            val Some(Expr(ETupleBody(ETuple(parentPs, _, _)))) = parentEntry.singleExpr()
            if (parentPs.length != 3)
              localFail()
            else {
              val Some(Expr(GByteArray(parentAdditional))) = parentPs(1).singleExpr
              val Some(Expr(GByteArray(lastKeyStr)))       = lastKey.singleExpr()
              val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = lastEntry.singleExpr()
              if (ps.length != 3)
                localFail()
              else {
                val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
                val mergeStream                            = ByteString.newOutput()
                parentAdditional.writeTo(mergeStream)
                lastKeyStr.writeTo(mergeStream)
                edgeAdditional.writeTo(mergeStream)
                val mergedEdge        = parByteArray(mergeStream.toByteString())
                val updatedTuple: Par = ETuple(Seq(ps(0), mergedEdge, ps(2)))
                val updatedMap: Par = ParMap(
                  SortedParMap(parMap.ps + (parentKeyPar -> updatedTuple))
                )
                replace(Quote(updatedMap), parentReplace, parentRand)
              }
            }
          }

          val Channel(Quote(dataPar))                  = data
          val Some(Expr(EMapBody(parMap)))             = dataPar.singleExpr
          val Some(value)                              = parMap.ps.get(parByteArray(head))
          val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = value.singleExpr()
          if (ps.length != 3)
            localFail()
          else {
            // The second tuple field should be a bytearray in both cases.
            val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
            ps(0).singleExpr() match {
              case Some(Expr(GInt(0))) =>
                if (tail == edgeAdditional) {
                  if (parMap.ps.size > 2) {
                    val updatedMap: Par = ParMap(SortedParMap(parMap.ps - parByteArray(head)))
                    for {
                      _ <- replace(Quote(updatedMap), replaceChan, dataRand)
                      _ <- replace(parentData, parentReplace, parentRand)
                      _ <- succeed(ps(2), ret, callRand)
                    } yield ()
                  } else if (parMap.ps.size != 2) {
                    localFail()
                  } else {
                    val shrunkMap = (parMap.ps - parByteArray(head))
                    for {
                      _ <- Function.tupled(mergeWithParent(_, _))(shrunkMap.head)
                      _ <- succeed(ps(2), ret, callRand)
                    } yield ()
                  }
                } else {
                  localFail()
                }
              case Some(Expr(GInt(1))) =>
                if (tail.startsWith(edgeAdditional)) {
                  val newKey = tail.substring(edgeAdditional.size)
                  fetchDataDelete(
                    Quote(ps(2)),
                    Channel(Quote(parByteArray(newKey))),
                    ret,
                    callRand,
                    Quote(parByteArray(head)),
                    data,
                    replaceChan,
                    dataRand
                  )
                } else {
                  localFail()
                }
            }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def publicLookup(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, ret), rand, cost)) =>
        def localFail() = fail(ret, rand)
        try {
          val Channel(Quote(keyPar)) = key
          val Some(Expr(GUri(uri)))  = keyPar.singleExpr
          if (uri.startsWith("rho:id:")) {
            val tail = uri.substring("rho:id:".length)
            if (tail.size != 54) {
              localFail()
            } else {
              // Could fail
              // 256 bits plus 14 bit crc-14
              val bytes: Array[Byte] = ZBase32.decode(tail, 270)
              val crc: Short =
                ((bytes(32).toShort & 0xff) | ((bytes(33).toShort & 0xfc) << 6)).toShort
              if (crc == CRC14.compute(bytes.view.slice(0, 32))) {
                val args = RootSeq(
                  ListChannelWithRandom(
                    Seq(Quote(parByteArray(ByteString.copyFrom(bytes, 0, 32))), ret),
                    rand
                  )
                )
                lookup(args)
              } else {
                localFail()
              }
            }
          } else {
            localFail()
          }
        } catch {
          case _: MatchError               => localFail()
          case _: IllegalArgumentException => localFail()
        }
      case _ => F.unit
    }

  def publicRegisterRandom(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(value, ret), rand, cost)) =>
        def localFail() = fail(ret, rand)
        try {
          val Channel(Quote(valPar)) = value
          if (valPar.serializedSize > 1024)
            localFail()
          else {
            val bytes           = rand.next()
            val partialKey: Par = parByteArray(ByteString.copyFrom(bytes))
            val curryChan: Par  = GPrivate(ByteString.copyFrom(rand.next()))
            val resultChan: Par = GPrivate(ByteString.copyFrom(rand.next()))
            val uriPar: Par     = GUri(buildURI(bytes))
            val args = RootSeq(
              ListChannelWithRandom(Seq(Quote(partialKey), Quote(valPar), Quote(resultChan)), rand)
            )
            for {
              _ <- handleResult(
                    space.produce(
                      Quote(curryChan),
                      // This re-use of rand is fine because we throw it away in the callback below.
                      ListChannelWithRandom(Seq(Quote(uriPar), value, ret), rand, None),
                      false
                    )
                  )
              _ <- handleResult(
                    space.consume(
                      Seq[Channel](Quote(curryChan), Quote(resultChan)),
                      publicRegisterInsertCallbackPatterns,
                      TaggedContinuation(ScalaBodyRef(publicRegisterInsertCallbackRef)),
                      false
                    )
                  )
              _ <- insert(args)
            } yield ()
          }
        } catch {
          case _: MatchError               => localFail()
          case _: IllegalArgumentException => localFail()
        }
      case _ => F.unit
    }

  def publicRegisterInsertCallback(args: RootSeq[ListChannelWithRandom]): F[Unit] =
    args match {
      case Seq(
          ListChannelWithRandom(Seq(urn, expectedValue, ret), _, callCost),
          ListChannelWithRandom(Seq(value), valRand, valCost)
          ) =>
        ret match {
          case Channel(retQ @ Quote(_)) =>
            if (expectedValue == value)
              singleSend(urn, retQ, valRand)
            else
              fail(ret, valRand)
          case _ =>
            fail(ret, valRand)
        }
      case _ =>
        F.unit
    }
}

object Registry {
  val registryRoot = GPrivate(
    ByteString
      .copyFrom(Base16.decode("a4fd447dedfc960485983ee817632cf36d79f45fd1796019edfb4a84a81d1697"))
  )

  def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  val testingUrnMap: Map[String, Par] = Map(
    "rho:registry:testing:lookup"  -> byteName(10),
    "rho:registry:testing:insert"  -> byteName(12),
    "rho:registry:testing:delete"  -> byteName(14),
    "rho:registry:lookup"          -> byteName(17),
    "rho:registry:insertArbitrary" -> byteName(18)
  )

  object FixedRefs {
    val lookupRef: Long               = Runtime.BodyRefs.REG_LOOKUP
    val lookupCallbackRef: Long       = Runtime.BodyRefs.REG_LOOKUP_CALLBACK
    val insertRef: Long               = Runtime.BodyRefs.REG_INSERT
    val deleteRef: Long               = Runtime.BodyRefs.REG_DELETE
    val insertCallbackRef: Long       = Runtime.BodyRefs.REG_INSERT_CALLBACK
    val deleteRootCallbackRef: Long   = Runtime.BodyRefs.REG_DELETE_ROOT_CALLBACK
    val deleteCallbackRef: Long       = Runtime.BodyRefs.REG_DELETE_CALLBACK
    val publicLookupRef: Long         = Runtime.BodyRefs.REG_PUBLIC_LOOKUP
    val publicRegisterRandomRef: Long = Runtime.BodyRefs.REG_PUBLIC_REGISTER_RANDOM
    val publicRegisterInsertCallbackRef: Long =
      Runtime.BodyRefs.REG_PUBLIC_REGISTER_INSERT_CALLBACK

  }

  object CRC14 {
    val INIT_REMAINDER: Short = 0
    def update(rem: Short, b: Byte): Short = {
      @tailrec
      def loop(i: Int, rem: Short): Short =
        if (i < 8) {
          val shiftRem: Short = (rem << 1).toShort
          if ((shiftRem & 0x4000) != 0)
            loop(i + 1, (shiftRem ^ 0x4805).toShort)
          else
            loop(i + 1, shiftRem)
        } else {
          rem
        }
      loop(0, (rem ^ (b << 6).toShort).toShort)
    }

    def compute(b: IndexedSeq[Byte]) =
      b.foldLeft(INIT_REMAINDER)(update(_, _))
  }

  def buildURI(arr: Array[Byte]): String = {
    val fullKey = new Array[Byte](34)
    Array.copy(arr, 0, fullKey, 0, 32)
    val crc = CRC14.compute(fullKey.view.slice(0, 32))
    fullKey(32) = (crc & 0xff).toByte
    fullKey(33) = ((crc & 0xff00) >>> 6).toByte
    "rho:id:" + ZBase32.encodeToString(fullKey, 270)
  }
}
