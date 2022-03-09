package coop.rchain.models.manual

final case class BindPattern(
    patterns: Seq[Par] = Seq.empty,
    remainder: Option[Var] = None,
    freeCount: Int = 0
)
