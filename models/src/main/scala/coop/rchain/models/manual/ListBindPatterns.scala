package coop.rchain.models.manual

final case class ListBindPatterns(
    patterns: Seq[BindPattern] = Seq.empty
)
