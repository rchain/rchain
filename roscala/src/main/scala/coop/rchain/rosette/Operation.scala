package coop.rchain.rosette

import cats.data.State._
import coop.rchain.rosette.Ctxt.{Continuation, CtxtTransition}

case class StdOprn(meta: Ob, parent: Ob, override val extension: StdExtension) extends Actor {
  override def dispatch: VMTransition[(Result[Ob], Option[Continuation])] =
    for {
      optArg0 <- inspect[VMState, Option[Ob]](_.ctxt.arg(0))

      result <- optArg0 match {
                 case Some(ob) => ob.lookupAndInvoke
                 case None =>
                   pure[VMState, (Result[Ob], Option[Continuation])](
                     (Left(RuntimeError("no argument for dispatch")), None))
               }
    } yield result
}

object StdOprn {
  def apply(extension: StdExtension): StdOprn = StdOprn(meta = null, parent = null, extension)

  val OprnVmError = StdOprn(null)
}
