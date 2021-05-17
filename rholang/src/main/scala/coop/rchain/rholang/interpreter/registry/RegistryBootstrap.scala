package coop.rchain.rholang.interpreter.registry

import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.SystemProcesses.FixedChannels

object RegistryBootstrap {
  val AST: Par = Par(
    news = Vector(
      bootstrap(FixedChannels.REG_LOOKUP),
      bootstrap(FixedChannels.REG_INSERT_RANDOM),
      bootstrap(FixedChannels.REG_INSERT_SIGNED)
    )
  )

  /**
    * This is used to get a one-time hold of write-only-bundled fixed
    * channel, e.g `FixedChannels.REG_LOOKUP`, from within Rholang code.
    * It can be used to produce a contract, e.g. the registry lookup
    * contract, on a write-only-bundled fixed channel.
    */
  private def bootstrap(channel: Par): New =
    New(
      bindCount = 1,
      p = Par(
        receives = Vector(
          // for (x <- channel) { x!(channel) }
          Receive(
            binds = Vector(
              // for (x <- channel)
              ReceiveBind(
                patterns = Vector(EVar(FreeVar(0))),
                source = channel,
                freeCount = 1
              )
            ),
            body = Par(
              // x!(channel)
              sends = Vector(
                Send(
                  chan = EVar(BoundVar(0)),
                  data = Vector(channel)
                )
              )
            ),
            bindCount = 1
          )
        )
      )
    )
}
