package coop.rchain.rosette

import cats.data.State._
import coop.rchain.rosette.Ctxt.{Continuation, CtxtTransition}

case class StdOprn(meta: Ob, parent: Ob, override val extension: StdExtension) extends Actor {
  override def dispatch: CtxtTransition[(Result, Option[Continuation])] =
    for {
      optArg0 <- inspect[Ctxt, Option[Ob]](_.arg(0))

      result <- optArg0 match {
                 case Some(ob) => ob.lookupAndInvoke
                 case None =>
                   pure[Ctxt, (Result, Option[Continuation])](
                     (Left(RuntimeError("no argument for dispatch")), None))
               }
    } yield result
}

object StdOprn {
  def apply(extension: StdExtension): StdOprn = StdOprn(meta = null, parent = null, extension)

  val OprnVmError = StdOprn(null)
}
