package coop.rchain.models.manual

/** *
  * String interpolation
  *
  * `"Hello, {name}" %% {"name": "Bob"}` denotes `"Hello, Bob"`
  */
final case class EPercentPercent(
    p1: Par = Par.defaultInstance,
    p2: Par = Par.defaultInstance
)
