package coop.rchain.rosette

import cats.implicits._

case class StdOprn(meta: Ob, parent: Ob, override val extension: StdExtension) extends Actor {
  override def dispatch: CtxtTransition[Result[Ob]] =
    for {
      ctxt    <- getCtxt
      optArg0 = ctxt.arg(0)

      result <- optArg0 match {
                 case Some(ob) => ob.lookupAndInvoke
                 case None     => pureCtxt[Result[Ob]](Left(RuntimeError("no argument for dispatch")))
               }
    } yield result
}

object StdOprn {
  def apply(extension: StdExtension): StdOprn = StdOprn(meta = null, parent = null, extension)

  val OprnVmError = StdOprn(null)
}
