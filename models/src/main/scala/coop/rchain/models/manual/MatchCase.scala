package coop.rchain.models.manual

final case class MatchCase(
    pattern: Par = Par.defaultInstance,
    source: Par = Par.defaultInstance,
    freeCount: Int = 0
)
