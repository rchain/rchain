package coop.rchain.rosette

import cats.data.State._
import coop.rchain.rosette.Ctxt.CtxtTransition

object Meta {
  case class StdMeta(override val extension: StdExtension = null) extends Actor {
    // TODO:
    def get(client: Ob, key: Ob): CtxtTransition[Result] =
      pure[Ctxt, Result](Left(Absent))

    // TODO:
    def lookupOBOStdMeta(client: Ob, key: Ob): CtxtTransition[Result] =
      for {
        result <- get(client, key)
      } yield {
        result.left.flatMap {
          // TODO:
          // BASE(BASE(client)->parent())->lookup(key, ctxt)
          case Absent if client.isInstanceOf[RblFloat] =>
            Right(prim.Prims(197))
          case Absent => Right(prim.Prims(226))
        }
      }
  }
}
