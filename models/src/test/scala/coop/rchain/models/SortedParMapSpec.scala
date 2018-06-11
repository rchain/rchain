package coop.rchain.models

import org.scalatest.{FlatSpec, Matchers}

class SortedParMapSpec extends FlatSpec with Matchers {

  private[this] def serializeEMap(map: Map[Par, Par]): Array[Byte] =
    EMap()

  "SortedParMap" should "preserve structure during round trip protobuf serialization" in {
    pending
  }

  it should "deduplicate elements where last seen element wins" in {
    pending
  }

  it should "preserve ordering during serialization" in {
    pending
  }

}
