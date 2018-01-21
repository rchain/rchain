package coop.rchain.rosette

import java.io.{File, PrintWriter}

import shapeless.Lens

package object utils {
  def printToFile(f: File)(op: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  class unsafeCastLens[B] {
    def apply[T, A](lens: Lens[T, A]): Lens[T, B] =
      new Lens[T, B] {
        override def get(s: T): B = lens.get(s).asInstanceOf[B]
        override def set(s: T)(b: B): T = lens.set(s)(b.asInstanceOf[A])
      }
  }

  object unsafeCastLens {
    def apply[B] = new unsafeCastLens[B]
  }

  def lensTrans[S, A](lens: Lens[S, A], s: S)(f: A => A): S = {
    val a = lens.get(s)
    val b = f(a)
    lens.set(s)(b)
  }
}
