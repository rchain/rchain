package coop.rchain.rosette.prim

import cats.implicits._
import coop.rchain.rosette._
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.{Ctxt, CtxtTransition, Ob}
import coop.rchain.rosette.Ob.Lenses._

object ctxt {
  object ctxtRtn extends Prim {
    override val name: String = "ctxt-rtn"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    override def fn: CtxtTransition[PrimResult] =
      for {
        ctxt    <- getCtxt
        optArg0 = ctxt.arg(0).map(_.asInstanceOf[Ctxt])
        optArg1 = ctxt.arg(1)

        result <- (optArg0, optArg1) match {
                   // Both ARG(0) and ARG(1) are available
                   case (Some(arg0), Some(arg1)) =>
                     for (returnRes <- returnResultToCtxt(arg0, arg1))
                       yield Right[PrimError, Ob](if (returnRes) Ob.INVALID else Ob.NIV)

                   case _ =>
                     pureCtxt[PrimResult](Left(TypeMismatch(0, Ctxt.getClass.getName)))
                 }

      } yield result

    private def returnResultToCtxt(k: Ctxt, result: Ob): CtxtTransition[Boolean] =
      for {
        ctxt                        <- getCtxt
        globalEnv                   <- getGlobalEnv
        (conts, (_, kUpdated), res) = Ctxt.ret(result).run((), (globalEnv, k)).value
        _                           <- modifyCtxt(_.update(_ >> 'argvec >> 'elem)(_.updated(0, kUpdated)))
        _                           <- tellCont(conts)
      } yield res
  }
}
