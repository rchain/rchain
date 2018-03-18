package coop.rchain.storage.test

/** An example pattern type for use in [[coop.rchain.storage.StorageActionsTests]]
  */
sealed trait Pattern extends Product with Serializable {

  def isMatch(a: Any): Boolean =
    this match {
      case Wildcard           => true
      case StringMatch(value) => value == a
    }
}
final case class StringMatch(value: String) extends Pattern
case object Wildcard                        extends Pattern
