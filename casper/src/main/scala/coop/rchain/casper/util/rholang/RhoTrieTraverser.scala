package coop.rchain.casper.util.rholang

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.crypto.hash.Keccak256
import coop.rchain.models.Expr.ExprInstance.{
  EListBody,
  EMapBody,
  ETupleBody,
  GByteArray,
  GInt,
  GString
}
import coop.rchain.models.GUnforgeable.UnfInstance.GPrivateBody
import coop.rchain.models.{EList, ETuple, Expr, GPrivate, GUnforgeable, Par, ParMap}
import coop.rchain.rholang.interpreter.{RhoRuntime, RhoType}
import coop.rchain.rholang.interpreter.storage.serializePar
import coop.rchain.shared.Serialize

/**
  * Traverse a Rholang Trie.
  * https://github.com/rchain/rchain/blob/19880674b9c50aa29efe91d77f70b06b861ca7a8/casper/src/main/resources/Registry.rho
  * According to the trie implemenetation in rholang, the methods below are hacks in scala to traverse the trie.
  */
object RhoTrieTraverser {
  def keccakHash(input: Array[Byte]): Par = RhoType.ByteArray(Keccak256.hash(input))
  val depthEach                           = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  val firstDepth                          = depthEach.map(Vector(_))
  val powers =
    List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536)

  /**
    * Scala version of https://github.com/rchain/rchain/blob/19880674b9c50aa29efe91d77f70b06b861ca7a8/casper/src/main/resources/Registry.rho#L72-L78
    */
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
  def nthOfPar(p: Par, nth: Int): Int = p.exprs(0).exprInstance match {
    case GByteArray(bs) if (0 <= nth && nth < bs.size) => {
      bs.byteAt(nth) & 0xff // convert to unsigned
    }
    case _ => throw new Exception(s"Par ${p} is not valid for nthOfPar method")
  }

  def parString(s: String) = Par(exprs = Seq(Expr(GString(s))))

  def parToByteArray(p: Par) = Serialize[Par].encode(p).toArray

  def keccakKey(s: String) = keccakHash(parToByteArray(parString(s)))

  def keccakParString(s: String) = Keccak256.hash(parToByteArray(parString(s)))

  def nodeList(nybList: Vector[Int]) =
    Par(
      exprs =
        Seq(Expr(EListBody(EList(ps = nybList.map(n => Par(exprs = Seq(Expr(GInt(n.toLong)))))))))
    )

  def nodeMapList(map: Par, nybList: Par) = {
    val mapWithNyb = Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(map, nybList))))))
    nodeMapStore(mapWithNyb)
  }
  def nodeMapList(map: Par, nybList: Vector[Int]) = {
    val mapWithNyb = Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(map, nodeList(nybList)))))))
    nodeMapStore(mapWithNyb)
  }
  def nodeMapStore(mapWithNyb: Par) =
    Par(exprs = Seq(Expr(EListBody(EList(ps = Seq(mapWithNyb, storeTokenUnforgeable))))))

  val storeTokenUnforgeable = {
    val rand =
      Tools.unforgeableNameRng(StandardDeploys.registry.pk, StandardDeploys.registry.data.timestamp)
    (0 to 5).foreach(_ => rand.next())
    val newRand = rand.splitShort(6)
    (0 to 6).foreach(_ => newRand.next())
    val target = newRand.next()
    Par(unforgeables = Seq(GUnforgeable(GPrivateBody(GPrivate(id = ByteString.copyFrom(target))))))
  }

  def TreeHashMapGetter[F[_]: Sync](mapPar: Par, nybList: Vector[Int], runtime: RhoRuntime[F]) =
    for {
      result <- runtime.getData(nodeMapList(mapPar, nybList))
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
      Par,
      RhoRuntime[F],
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
      runtime: RhoRuntime[F]
  ): F[Vector[ParMap]] = {
    val startParams: ReadParams[F] =
      (Vector(Vector.empty), depth * 2, mapPar, runtime, Vector.empty)
    startParams.tailRecM(traverseTrieRec[F])
  }

  def extendKey(head: Vector[Int], value: Long) =
    depthEach.filter(i => (value / powers(i)) % 2 != 0).map(i => head ++ Vector(i))

  private def traverseTrieRec[F[_]: Sync](
      readParams: ReadParams[F]
  ): F[Either[ReadParams[F], Vector[ParMap]]] = {
    val (keys, depth, mapPar, runtime, collectedResults) = readParams
    keys match {
      case key +: keyRests =>
        for {
          currentNode <- TreeHashMapGetter(mapPar, key, runtime)
          result <- currentNode match {
                     case Some(Left(i)) =>
                       if (key.isEmpty)
                         Left(
                           keyRests ++ extendKey(key, i),
                           depth,
                           mapPar,
                           runtime,
                           collectedResults
                         ).pure[F]
                       else if (depth == key.length)
                         Left(keyRests, depth, mapPar, runtime, collectedResults).pure[F]
                       else
                         Left(
                           keyRests ++ extendKey(key, i),
                           depth,
                           mapPar,
                           runtime,
                           collectedResults
                         ).pure[F]
                     case Some(Right(map)) =>
                       Left(keyRests, depth, mapPar, runtime, map +: collectedResults).pure[F]
                     case _ => Left(keyRests, depth, mapPar, runtime, collectedResults).pure[F]
                   }
        } yield result
      case _ => collectedResults.asRight[ReadParams[F]].pure[F]
    }

  }
}
