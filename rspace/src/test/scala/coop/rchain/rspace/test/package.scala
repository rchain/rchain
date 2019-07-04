package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats._
import cats.implicits._
import cats.effect.{Concurrent, ContextShift, ExitCase, Fiber}
import coop.rchain.shared.Language.ignore
import monix.eval.Coeval

import scala.concurrent.ExecutionContext
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult}

package object test {

  def roundTripCodec[T](t: T)(implicit codec: Codec[T]): Attempt[DecodeResult[T]] =
    codec.encode(t).flatMap((vector: BitVector) => codec.decode(vector))

  import scala.reflect.ClassTag
  def collectActions[HA <: HotStoreAction: ClassTag](changes: Seq[HotStoreAction]): Seq[HA] = {
    val clazz = implicitly[ClassTag[HA]].runtimeClass
    changes
      .collect {
        case e: HA if clazz.isInstance(e) => e
      }
  }

  implicit class StoreOps[F[_]: Functor, C, P, A, K](val store: HotStore[F, C, P, A, K]) {
    def isEmpty(): F[Boolean] =
      store.changes().map(collectActions[InsertAction]).map(_.isEmpty)
  }
}
