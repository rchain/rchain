package coop.rchain.models.manual

import coop.rchain.models.Var.VarInstance

/** While we use vars in both positions, when producing the normalized
  * representation we need a discipline to track whether a var is a name or a
  * process.
  * These are DeBruijn levels
  */
final case class Var(
    varInstance: VarInstance = VarInstance.Empty
)

object Var {
  lazy val defaultInstance: Var = Var(
    varInstance = VarInstance.Empty
  )
}
