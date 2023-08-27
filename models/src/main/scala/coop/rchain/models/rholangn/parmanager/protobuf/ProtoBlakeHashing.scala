package coop.rchain.models.rholangn.parmanager.protobuf

import cats.Eval
import cats.syntax.all._
import coop.rchain.models.rholangn.ParN._
import coop.rchain.models.rholangn.parmanager.blake2.Blake2Hash
import coop.rchain.models.rholangn.parmanager.protobuf.ProtoPrimitiveWriter._

import java.io.ByteArrayOutputStream
import scala.util.Using

/** Hashing functions of primitive types using protobuf encoder and Blake2Hash */
object ProtoBlakeHashing {

  /** Main hash function, using Blake2Hash */
  def hash(x: Array[Byte]): Array[Byte] = Blake2Hash.hash(x)

  /* Hash functions of primitive types. */

  val HASH_TRUE: Eval[Array[Byte]]  = encodeWith(_.write(true)).map(hash).memoize
  val HASH_FALSE: Eval[Array[Byte]] = encodeWith(_.write(false)).map(hash).memoize

  def hash(x: Boolean): Eval[Array[Byte]] = if (x) HASH_TRUE else HASH_FALSE
  def hash(x: Byte): Eval[Array[Byte]]    = encodeWith(_.write(x)).map(hash)
  def hash(x: Int): Eval[Array[Byte]]     = encodeWith(_.write(x)).map(hash)
  def hash(x: Long): Eval[Array[Byte]]    = encodeWith(_.write(x)).map(hash)
  def hash(x: String): Eval[Array[Byte]]  = encodeWith(_.write(x)).map(hash)

  /** Concatenates bytes arrays using [[ByteArrayOutputStream]] to minimize Array resizing. */
  // TODO: Properly handle errors
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def concatSeq(seq: Seq[Array[Byte]]): Array[Byte] =
    Using(new ByteArrayOutputStream(512)) { out =>
      seq.foreach(out.write)
      out.flush()
      out.toByteArray
    }.get

  /** Helper combinators to define hashing of Rholang AST types. */
  implicit class EvalArrayCombinators(val seq1: Eval[Array[Byte]]) extends AnyVal {

    /** ===Concatenates two byte arrays===
      *
      * {{{ ++ : Eval[Array[Byte]] => Eval[Array[Byte]] => Eval[Array[Byte]] }}}
      * Concatenates two byte arrays. It works the same as Scala concat (`++`) function, but wrapped in `Eval`.
      */
    @inline def ++(seq2: Eval[Array[Byte]]): Eval[Array[Byte]] =
      (seq1, seq2).mapN(_ ++ _)

    /** ===Prepends a byte array to the sequence of byte arrays===
      *
      * {{{ +++ : Eval[Array[Byte]] => Eval[Seq[Array[Byte]] => Eval[Array[Byte]] }}}
      * Prepends a byte array to the sequence of byte arrays, concatenates the sequence and computes the final hash.
      */
    @inline def +++(seq2: Eval[Seq[Array[Byte]]]): Eval[Array[Byte]] =
      (seq1, seq2).mapN(_ +: _).map(concatSeq).map(hash)

    /** ===Prepends a byte array to the sequence of byte arrays with sorting===
      *
      * {{{ +|+ : Eval[Array[Byte]] => Eval[Seq[Array[Byte]] => Eval[Array[Byte]] }}}
      * The same as `+++`, but the sequence is first sorted before concatenation.
      */
    @inline def +|+(seq2: Eval[Seq[Array[Byte]]]): Eval[Array[Byte]] =
      (seq1, seq2).mapN(_ +: _).map(_.sorted).map(concatSeq).map(hash)
  }
}
