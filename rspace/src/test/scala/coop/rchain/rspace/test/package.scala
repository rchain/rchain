package coop.rchain.rspace

import cats._
import cats.syntax.all._

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
