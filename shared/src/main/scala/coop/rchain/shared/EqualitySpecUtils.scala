package coop.rchain.shared

object EqualitySpecUtils {

  // As long as all the components of the tested object are constructed in the `value` thunk,
  // this will catch just comparing the references (the test will fail).
  // TODO: move to tests, it's only used there
  def checkValueBasedEquality(nonCollidingValues: => Seq[Any]) = {
    val lefts  = nonCollidingValues
    val rights = nonCollidingValues
    lefts.zip(rights).foreach {
      case (l, r) =>
        assert(l == r, s"$l == $r")
        assert(l.hashCode() == r.hashCode(), s"${l.hashCode()} == ${r.hashCode()}")
    }
    val n = null
    object SurelyNotEqual //hopefully not causing a hash collision too...
    // This odd Seq typed to Any is due to compiler does not like type inferring to Any
    // Error in migration to Scala 2.13
    // a type was inferred to be `Any`; this may indicate a programming error.
    // [error]     (n +: SurelyNotEqual +: lefts).combinations(2).foreach {
    val items: Seq[Any] = Seq.empty[Any] :+ n :+ SurelyNotEqual +: lefts
    items.combinations(2).foreach {
      case l +: r +: Nil =>
        assert(l != r, s"$l != $r")
        assert(r != l, s"$r != $l")
        assert(
          l == null || r == null || l.hashCode() != r.hashCode(),
          s"${l.hashCode()} != ${r.hashCode()}"
        )
    }
  }
  ()
}
