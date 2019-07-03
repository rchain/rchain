package coop.rchain.shared

object EqualitySpecUtils {

  // As long as all the components of the tested object are constructed in the `value` thunk,
  // this will catch just comparing the references (the test will fail).
  def checkValueBasedEquality[A](nonCollidingValues: => Seq[A]) = {
    val lefts  = nonCollidingValues
    val rights = nonCollidingValues
    lefts.zip(rights).foreach {
      case (l, r) =>
        assert(l == r, s"$l == $r")
        assert(l.hashCode() == r.hashCode(), s"${l.hashCode()} == ${r.hashCode()}")
    }
    val n = null
    object SurelyNotEqual //hopefully not causing a hash collision too...
    (n +: SurelyNotEqual +: lefts).combinations(2).foreach {
      case l :: r :: Nil =>
        assert(l != r, s"$l != $r")
        assert(r != l, s"$r != $l")
        assert(
          l == null || r == null || l.hashCode() != r.hashCode(),
          s"${l.hashCode()} != ${r.hashCode()}"
        )
    }
  }
}
