package coop.rchain.casper.util.rholang

import coop.rchain.models.PCost
import coop.rchain.rspace.trace.Event

final case class InternalProcessedSystemDeploy(cost: PCost, eventLog: Seq[Event])