package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random

/** *
  * Rholang code along with the state of a split random number
  * generator for generating new unforgeable names.
  */
final case class ParWithRandom(
    body: Par = Par.defaultInstance,
    randomState: Blake2b512Random = RhoTypesConst._typemapper_randomState.toCustom(ByteString.EMPTY)
)
