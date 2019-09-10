package coop.rchain.rholang.interpreter
import coop.rchain.models.Par

final case class DeployParameters(userId: Par)

object DeployParameters {
  def empty: DeployParameters = DeployParameters(Par())
}
