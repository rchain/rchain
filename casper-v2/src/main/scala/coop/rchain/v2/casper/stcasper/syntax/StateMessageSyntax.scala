package coop.rchain.v2.casper.stcasper.syntax
import coop.rchain.v2.casper.stcasper.StateMessage

trait StateMessageSyntax {
  implicit final def stateMessageSyntax[U](c: StateMessage[U]): StateMessageOps[U] =
    new StateMessageOps[U](c)
}

final class StateMessageOps[U](val thiz: StateMessage[U]) extends AnyVal {
  def conflicts(that: StateMessage[U]): Boolean = StateMessage.conflicts(thiz, that)
}
