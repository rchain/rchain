package coop.rchain.rosette

import scala.collection.mutable

object Meta {
  case class StdMeta(override val extension: Ob = null,
                     override val _slot: mutable.Seq[Ob] = null)
      extends Actor {
    def get(client: Ob, key: Ob, ctxt: Ctxt): Either[RblError, Ob] =
      Left(Absent)

    def lookupOBOStdMeta(client: Ob, key: Ob)(
        state: VMState): Either[RblError, Ob] =
      if (state.interruptPending != 0) {
        Left(Absent)
      } else {
        val result = get(client, key, state.ctxt)

        result.left.flatMap {
          // TODO:
          // BASE(BASE(client)->parent())->lookup(key, ctxt)
          case Absent => Right(prim.Prims(226))
        }
      }
  }
}
