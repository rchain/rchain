package coop.rchain.rosette

object Meta {
  case class StdMeta(meta: Ob, parent: Ob, override val extension: StdExtension) extends Actor {
    // TODO:
    def get(client: Ob, key: Ob): CtxtTransition[Result] =
      pureCtxt[Result](Left(Absent))

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

  object StdMeta {
    def apply(extension: StdExtension = null): StdMeta =
      StdMeta(meta = null, parent = null, extension)
  }
}
