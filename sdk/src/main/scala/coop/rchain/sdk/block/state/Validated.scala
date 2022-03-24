package coop.rchain.sdk.block.state
import cats.syntax.all._
import coop.rchain.sdk.block.state.Validated._

/** State of validated DAG. */
final case class Validated[BId](st: ValidatedState[BId]) {
  type ST = Validated[BId]

  def add(b: BId, offenceOpt: Option[Offence]): (ST, ValidatedEffect[BId]) = {
    val newSt  = st                                              // TODO
    val effect = ValidatedEffect(none[Offence], List.empty[BId]) // TODO
    (Validated(newSt), effect)
  }

  // Prune to LFS
  def prune() = ??? // TODO

  def contains(bid: BId): Boolean = st.dagSet.contains(bid)
}

object Validated {
  // State definition
  final case class ValidatedState[BId](dagSet: Set[BId]) // TODO this is content of BlockDagRepresentation

  // Effects
  final case class ValidatedEffect[BId](offence: Option[Offence], childrenUnlocked: List[BId])

  trait Offence
  // Now in Casper code there is no distinction between offences, just invalid block.
  // In future would be good to store particular offence, but now it might be easier to use just this
  final case object UnknownOffence extends Offence
}
