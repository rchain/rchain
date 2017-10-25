package coop.rchain.rosette

case class StdOprn(override val extension: Ob,
                   override val parent: Ob,
                   override val meta: Ob,
                   override val slot: Seq[Ob])
    extends Actor

object OprnVmError extends StdOprn(null, null, null, null)
