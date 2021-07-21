package coop.rchain.rspace.merging

import org.scalatest.FlatSpec

class MergingLogicSpec extends FlatSpec {
  "produce copied by peek in one EventLogIndex and originated in another" should
    "be declared originated in combined EventLogIndex"

  "race for the same produce" should "be declared as conflict"
  "potential COMM event on merge" should "be declared as conflict"

  //TODO this is very restrictive, should not be much of an effort to relax
  "even single produce in any of EventLogIndex touching base join" should "declare conflict"
}
