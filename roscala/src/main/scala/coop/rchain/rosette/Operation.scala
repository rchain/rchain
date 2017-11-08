package coop.rchain.rosette

import scala.collection.mutable

case class StdOprn(override val extension: Ob,
                   override val _slot: mutable.Seq[Ob])
    extends Actor {
  override def dispatch(state: VMState): (Either[RblError, Ob], VMState) =
    // TODO:
    //if (debugging_level)
    //    printf("\toprn %s\n", BASE(extension->slot(STDOPRN_ID_SLOT))->asCstring());
    if (state.ctxt.nargs > 0) {
      state.ctxt
        .arg(0)
        .map(_.lookupAndInvoke(state))
        .getOrElse((Left(RuntimeError("no argument for dispatch")), state))
    } else {
      (Left(RuntimeError("no argument for dispatch")), state)
    }
}

object OprnVmError extends StdOprn(null, null)
