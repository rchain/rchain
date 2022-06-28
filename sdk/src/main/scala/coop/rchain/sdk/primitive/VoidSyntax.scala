package coop.rchain.sdk.primitive

trait VoidSyntax {
  implicit def sdkSyntaxVoid[A](a: A): VoidOps[A] = new VoidOps(a)
}

final class VoidOps[A](private val a: A) extends AnyVal {

  /**
    * Ignore value and return [[Unit]].
    */
  def void(): Unit = ()
}
