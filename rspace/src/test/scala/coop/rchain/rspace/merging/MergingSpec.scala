package coop.rchain.rspace.merging

import org.scalatest.FlatSpec

trait MergingSpec extends FlatSpec {

  "BlockIndex.create" should "compute put dependent deploys in a single index"
  "BlockIndex.create" should "compute put independent deploys into separate index"
  "StateChanges.compute" should "calculate valid changes"
}
