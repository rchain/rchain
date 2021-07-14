package coop.rchain.models.manual

final case class ReceiveBind(
    patterns: Seq[Par] = Seq.empty,
    source: Par = Par.defaultInstance,
    remainder: Option[Var] = None,
    freeCount: Int = 0
)
