package coop.rchain.rspace.merging

import org.scalatest.FlatSpec

class EventLogIndexSpec extends FlatSpec {
  // produces indexing
  "producesLinear" should "contain linear produces both commed and not commed"
  "producesPersistent" should "persistent produces both commed and not commed"
  "producesLinear and producesPersistent" should "not intersect"

  "producesConsumed" should "contain produces both linear and persistent that are consumed in COMM event"
  "producesPeeked" should "contain produces both linear and persistent that are peeked in COMM event"
  "producesConsumed and producesPeeked" should "not intersect"

  "producesCopiedByPeek" should "contain such and only such produces"
  "producesTouchingBaseJoins" should "contain such and only such produces"

  // consumes indexing
  "consumesLinearAndPeeks" should "contain linear consumes and peeks"
  "consumesPersistent" should "contain persistent consumes"
  "consumesLinearAndPeeks and consumesPersistent" should "not intersect"

  "consumesProduced" should "contain consumes met in COMM event: linear peeks and persistent"

  // general logic
  it should "contain all events from event log"
  it should "not contain events not present in event log"
}
