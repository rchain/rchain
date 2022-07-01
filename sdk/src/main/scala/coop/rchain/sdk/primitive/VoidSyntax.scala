package coop.rchain.sdk.primitive

trait VoidSyntax {
  implicit def sdkSyntaxVoid[A](a: A): VoidOps[A] = new VoidOps(a)
}

final class VoidOps[A](private val a: A) extends AnyVal {

  /**
    * Ignore value and return [[Unit]].
    *
    * Convenient way for exceptional cases where we want to ignore a value and have statement instead of expression.
    *
    * '''Also dangerous because hides potential unused value. Please use sparingly.'''
    */
  def void(): Unit = ()
}
