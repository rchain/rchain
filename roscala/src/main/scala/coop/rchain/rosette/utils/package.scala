package coop.rchain.rosette

import java.io.{File, PrintWriter}

import shapeless.Lens

package object utils {
  def printToFile(f: File)(op: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  //this function exist only to use `asInstanceOf`-like casts at lens level
  //so normally when we use code like:
  //    ob.asInstanceOf[StdExtension]
  //at lens level we can perform a similar cast:
  //    val l: Lens[Ob,Ob] = lens[Ob] >> 'extension
  //    val extL: Lens[Ob,StdExtension] = new unsafeCastLens[StdExtension]()
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
