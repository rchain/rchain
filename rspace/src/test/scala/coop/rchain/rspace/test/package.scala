package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.Id
import cats.effect.ContextShift
import coop.rchain.rspace.history.{Branch, ITrieStore, Leaf, Node, Skip, Trie}
import coop.rchain.shared.Language.ignore
import scala.concurrent.ExecutionContext
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult}

package object test {

  implicit val contextShiftId: ContextShift[Id] =
    new ContextShift[Id] {
      def shift: Id[Unit]                                   = ???
      def evalOn[A](ec: ExecutionContext)(fa: Id[A]): Id[A] = fa
    }

  /**
    * Converts specified byteBuffer to '-' separated string,
    * convenient during debugging
    */
  private[rspace] def toStr(byteBuffer: ByteBuffer): String = {
    byteBuffer.mark()
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore { byteBuffer.get(fetched) }
    byteBuffer.reset()
    fetched.toSeq.map(x => x.toString).mkString("-")
  }

  def roundTripCodec[T](t: T)(implicit codec: Codec[T]): Attempt[DecodeResult[T]] =
    codec.encode(t).flatMap((vector: BitVector) => codec.decode(vector))

  def offset(d: Int) = ("   " * d)

  def printTree[T, K, V](store: ITrieStore[T, K, V], branch: Branch = Branch.MASTER): Unit =
    store.withTxn(store.createTxnRead()) { txn =>
      def printBranch(d: Int, t: Trie[K, V]): Unit =
        t match {
          case Leaf(key, value) => println(offset(d), "Leaf", key, value)
          case Node(pointerBlock) =>
            println(offset(d), "node")
            pointerBlock.childrenWithIndex.foreach {
              case (p, i) =>
                val n = store.get(txn, p.hash).get
                println(offset(d), i, "#", p.hash)
                printBranch(d + 1, n)
            }
          case Skip(affix, p) =>
            val n = store.get(txn, p.hash).get
            println(offset(d), "skip", affix, "#", p.hash)
            printBranch(d + 1, n)
        }
      val root = store.getRoot(txn, branch)
      println("---------------------")
      println("root#", root)
      val rootNode = store.get(txn, root.get).get
      printBranch(0, rootNode)
      println("---------------------")
    }

}
