package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.Id
import cats.effect.{Concurrent, ContextShift, ExitCase, Fiber}
import coop.rchain.rspace.history.{Branch, ITrieStore, Leaf, Node, Skip, Trie}
import coop.rchain.shared.Language.ignore
import monix.eval.Coeval

import scala.concurrent.ExecutionContext
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult}

package object test {

  implicit val concurrentCoeval: Concurrent[Coeval] = new Concurrent[Coeval] {
    val catsSync                                                   = Coeval.catsSync
    override def start[A](fa: Coeval[A]): Coeval[Fiber[Coeval, A]] = ???
    override def racePair[A, B](
        fa: Coeval[A],
        fb: Coeval[B]
    ): Coeval[Either[(A, Fiber[Coeval, B]), (Fiber[Coeval, A], B)]]                      = ???
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Coeval[A]          = ???
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => Coeval[Unit]): Coeval[A] = ???
    override def suspend[A](thunk: => Coeval[A]): Coeval[A]                              = catsSync.suspend(thunk)
    override def bracketCase[A, B](acquire: Coeval[A])(use: A => Coeval[B])(
        release: (A, ExitCase[Throwable]) => Coeval[Unit]
    ): Coeval[B]                          = catsSync.bracketCase(acquire)(use)(release)
    override def pure[A](x: A): Coeval[A] = catsSync.pure(x)
    override def flatMap[A, B](fa: Coeval[A])(f: A => Coeval[B]): Coeval[B] =
      catsSync.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => Coeval[Either[A, B]]): Coeval[B] =
      catsSync.tailRecM(a)(f)
    override def raiseError[A](e: Throwable): Coeval[A] = catsSync.raiseError(e)
    override def handleErrorWith[A](fa: Coeval[A])(f: Throwable => Coeval[A]): Coeval[A] =
      catsSync.handleErrorWith(fa)(f)
  }

  implicit val contextShiftId: ContextShift[Id] =
    new ContextShift[Id] {
      def shift: Id[Unit]                                   = ???
      def evalOn[A](ec: ExecutionContext)(fa: Id[A]): Id[A] = fa
    }

  implicit val contextShiftCoeval: ContextShift[Coeval] =
    new ContextShift[Coeval] {
      def shift: Coeval[Unit]                                       = ???
      def evalOn[A](ec: ExecutionContext)(fa: Coeval[A]): Coeval[A] = fa
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
