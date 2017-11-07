package coop.rchain.rosette

import scala.collection.mutable

case class StdOprn(override val extension: Ob,
                   override val _slot: mutable.Seq[Ob])
    extends Actor

object OprnVmError extends StdOprn(null, null)
