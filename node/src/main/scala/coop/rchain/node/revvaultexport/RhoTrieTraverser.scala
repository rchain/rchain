package coop.rchain.node.revvaultexport

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.BlockRandomSeed
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.rholang.RuntimeManager.emptyStateHashFixed
import coop.rchain.casper.rholang.Tools
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.GUnforgeable.UnfInstance.GPrivateBody
import coop.rchain.models._
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.storage.serializePar
import coop.rchain.rholang.interpreter.{RhoRuntime, RhoType}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Serialize

import scala.annotation.tailrec
import scala.collection.compat.immutable.LazyList

/**
  * Traverse a Rholang Trie.
  * https://github.com/rchain/rchain/blob/19880674b9c50aa29efe91d77f70b06b861ca7a8/casper/src/main/resources/Registry.rho
  * According to the trie implemenetation in rholang, the methods below are hacks in scala to traverse the trie.
  */
object RhoTrieTraverser {
  private def keccakHash(input: Array[Byte]): Par = RhoType.ByteArray(Keccak256.hash(input))
  private val depthEach                           = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  private val powers =
    List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536)

  /**
    * Scala version of https://github.com/rchain/rchain/blob/19880674b9c50aa29efe91d77f70b06b861ca7a8/casper/src/main/resources/Registry.rho#L72-L78
    */
  @tailrec
  def byteArrayToNybbleList(
      binaryArray: Par,
      n: Int,
      length: Int,
      acc: Vector[Int]
  ): Vector[Int] =
    if (n == length) acc
    else
      byteArrayToNybbleList(
        binaryArray,
        n + 1,
        length,
        acc ++ Vector(nthOfPar(binaryArray, n) % 16, nthOfPar(binaryArray, n) / 16)
      )

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def nthOfPar(p: Par, nth: Int): Int = p.exprs.head.exprInstance match {
    case GByteArray(bs) if 0 <= nth && nth < bs.size =>
      bs.byteAt(nth) & 0xff // convert to unsigned
    case _ => throw new Exception(s"Par $p is not valid for nthOfPar method")
  }

  private def parString(s: String) = Par(exprs = Seq(Expr(GString(s))))

  private def parToByteArray(p: Par) = Serialize[Par].encode(p).toArray

  def keccakKey(s: String): Par = keccakHash(parToByteArray(parString(s)))

  def keccakParString(s: String): Array[Byte] = Keccak256.hash(parToByteArray(parString(s)))

  def nodeList(nybList: Vector[Int]): Par =
    Par(
      exprs =
        Seq(Expr(EListBody(EList(ps = nybList.map(n => Par(exprs = Seq(Expr(GInt(n.toLong)))))))))
    )

  private def nodeMapList(map: Par, storeTokenPar: Par, nybList: Vector[Int]) = {
    val mapWithNyb = Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(map, nodeList(nybList)))))))
    nodeMapStore(mapWithNyb, storeTokenPar)
  }
  private def nodeMapStore(mapWithNyb: Par, storeTokenPar: Par) =
    Par(exprs = Seq(Expr(EListBody(EList(ps = Seq(mapWithNyb, storeTokenPar))))))

  def storeTokenUnforgeable(shardId: String, validatorKey: PublicKey): Par = {
    val rand = BlockRandomSeed(
      shardId,
      0,
      validatorKey,
      Blake2b256Hash.fromByteString(emptyStateHashFixed)
    ).generateRandomNumber.splitByte(0.toByte).splitByte(BlockRandomSeed.UserDeploySplitIndex)
    val target = LazyList.continually(rand.next()).drop(9).head
    target.toParUnforgeableName
  }

  private def TreeHashMapGetter[F[_]: Sync](
      mapPar: Par,
      storeTokenPar: Par,
      nybList: Vector[Int],
      runtime: RhoRuntime[F]
  ) =
    for {
      result <- runtime.getData(nodeMapList(mapPar, storeTokenPar, nybList))
      r = result match {
        case head +: _ =>
          head.a.pars match {
            case headPar +: Nil =>
              headPar.exprs match {
                case headExpr +: Nil =>
                  headExpr.exprInstance match {
                    case GInt(i)         => Some(Left(i))
                    case EMapBody(value) => Some(Right(value))
                    case _               => None
                  }
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }
    } yield r

  type ReadParams[F[_]] = (
      Vector[Vector[Int]],
      Int,
      Vector[ParMap]
  )

  def vecParMapToMap[K, V](
      values: Vector[ParMap],
      getKey: Par => K,
      getValue: Par => V
  ): Map[K, V] =
    values.flatMap(m => m.ps.map { case (kPar, vPar) => (getKey(kPar), getValue(vPar)) }).toMap

  def traverseTrie[F[_]: Sync](
      depth: Int,
      mapPar: Par,
      storeTokenPar: Par,
      runtime: RhoRuntime[F]
  ): F[Vector[ParMap]] = {
    val startParams: ReadParams[F] =
      (Vector(Vector.empty), depth * 2, Vector.empty)
    startParams.tailRecM(traverseTrieRec[F](mapPar, storeTokenPar, runtime))
  }

  private def extendKey(head: Vector[Int], value: Long) =
    depthEach.filter(i => (value / powers(i)) % 2 != 0).map(i => head ++ Vector(i))

  private def traverseTrieRec[F[_]: Sync](
      mapPar: Par,
      storeTokenPar: Par,
      runtime: RhoRuntime[F]
  )(
      readParams: ReadParams[F]
  ): F[Either[ReadParams[F], Vector[ParMap]]] = {
    val (keys, depth, collectedResults) = readParams
    keys match {
      case key +: keyRests =>
        for {
          currentNode <- TreeHashMapGetter(mapPar, storeTokenPar, key, runtime)
          result <- currentNode match {
                     case Some(Left(i)) =>
                       if (key.isEmpty)
                         Left(
                           keyRests ++ extendKey(key, i),
                           depth,
                           collectedResults
                         ).pure[F]
                       else if (depth == key.length)
                         Left(keyRests, depth, collectedResults).pure[F]
                       else
                         Left(
                           keyRests ++ extendKey(key, i),
                           depth,
                           collectedResults
                         ).pure[F]
                     case Some(Right(map)) =>
                       Left(keyRests, depth, map +: collectedResults).pure[F]
                     case _ => Left(keyRests, depth, collectedResults).pure[F]
                   }
        } yield result
      case _ => collectedResults.asRight[ReadParams[F]].pure[F]
    }

  }
}
