package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random

final case class ListParWithRandom(
    pars: Seq[Par] = Seq.empty,
    randomState: Blake2b512Random = RhoTypesConst._typemapper_randomState
      .toCustom(ByteString.EMPTY)
)
