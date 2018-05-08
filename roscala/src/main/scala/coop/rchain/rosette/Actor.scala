package coop.rchain.rosette

import coop.rchain.rosette.Ctxt.Continuation

abstract class Actor extends Ob {
  val extension: StdExtension

  // TODO:
  override def lookupAndInvoke: CtxtTransition[Result[Ob]] = ???
}
