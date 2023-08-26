package coop.rchain.models.rholangn.parmanager.protobuf

import cats.Eval
import cats.syntax.all._
import coop.rchain.models.rholangn.parmanager.blake2.Blake2Hash
import coop.rchain.models.rholangn.parmanager.protobuf.ProtoPrimitiveWriter._
import coop.rchain.models.rholangn.ParN._
import coop.rchain.models.rholangn.RhoTypeN

import java.io.ByteArrayOutputStream
import scala.util.Using

object ProtoBlakeHashing {

  /** Main hash function, using Blake2Hash */
  def hash(x: Array[Byte]): Array[Byte] = Blake2Hash.hash(x)

  val hashTrue: Eval[Array[Byte]]  = encodeWith(_.write(true)).map(hash)
  val hashFalse: Eval[Array[Byte]] = encodeWith(_.write(false)).map(hash)

  def hash(x: Boolean): Eval[Array[Byte]] = if (x) hashTrue else hashFalse

  def hash(x: Byte): Eval[Array[Byte]] = encodeWith(_.write(x)).map(hash)

  def hash(x: Int): Eval[Array[Byte]] = encodeWith(_.write(x)).map(hash)

  def hash(x: Long): Eval[Array[Byte]] = encodeWith(_.write(x)).map(hash)

  def hash(x: String): Eval[Array[Byte]] = encodeWith(_.write(x)).map(hash)

  def hash(p1: RhoTypeN, p2: RhoTypeN) = (p1.rhoHash, p2.rhoHash).sequence

  // TODO: Properly handle errors
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def concatSeq(seq: Seq[Array[Byte]]): Array[Byte] =
    Using(new ByteArrayOutputStream(512)) { out =>
      seq.foreach { bytes =>
        out.write(bytes)
      }
      out.flush()
      out.toByteArray
    }.get

  implicit class EvalArrayCombinators(val seq1: Eval[Array[Byte]]) extends AnyVal {
    @inline def ++(seq2: Eval[Array[Byte]]): Eval[Array[Byte]] =
      (seq1, seq2).mapN(_ ++ _)

    @inline def +++(seq2: Eval[Seq[Array[Byte]]]): Eval[Array[Byte]] =
      (seq1, seq2).mapN(_ +: _).map(concatSeq).map(hash)

    @inline def +|+(seq2: Eval[Seq[Array[Byte]]]): Eval[Array[Byte]] =
      (seq1, seq2).mapN(_ +: _).map(_.sorted).map(concatSeq).map(hash)
  }
}
