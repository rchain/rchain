package coop.rchain.models.manual

import coop.rchain.models.TaggedContinuation.TaggedCont

/** *
  * Either rholang code or code built in to the interpreter.
  */
final case class TaggedContinuation(
    taggedCont: TaggedCont = TaggedCont.Empty
)
