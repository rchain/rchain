package coop.rchain.models.manual

final case class KeyValuePair(
    key: Par = Par.defaultInstance,
    value: Par = Par.defaultInstance
)
