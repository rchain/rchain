package coop.rchain.rosette

import cats.data.State._
import coop.rchain.rosette.Ctxt.{Continuation, CtxtTransition}

abstract class Actor extends Ob {
  val extension: StdExtension

  // TODO:
  override def lookupAndInvoke: CtxtTransition[(Result, Option[Continuation])] = ???
}
