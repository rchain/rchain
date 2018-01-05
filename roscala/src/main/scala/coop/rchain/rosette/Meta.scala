package coop.rchain.rosette

object Meta {
  case class StdMeta(override val extension: StdExtension = null)
      extends Actor {
    def get(client: Ob, key: Ob, ctxt: Ctxt): Result =
      Left(Absent)

    def lookupOBOStdMeta(client: Ob, key: Ob)(state: VMState): Result =
      if (state.interruptPending != 0) {
        Left(Absent)
      } else {
        val result = get(client, key, state.ctxt)

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
