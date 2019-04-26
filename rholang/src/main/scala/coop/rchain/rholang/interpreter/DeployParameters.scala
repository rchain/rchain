package coop.rchain.rholang.interpreter
import coop.rchain.models.Par

final case class DeployParameters(codeHash: Par, phloRate: Par, userId: Par, timestamp: Par)

object DeployParameters {
  def empty: DeployParameters = DeployParameters(Par(), Par(), Par(), Par())
}
