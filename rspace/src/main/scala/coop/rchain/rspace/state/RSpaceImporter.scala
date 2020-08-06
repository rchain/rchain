package coop.rchain.rspace.state

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.state.TrieImporter

trait RSpaceImporter[F[_]] extends TrieImporter[F] {
  type KeyHash = Blake2b256Hash
}
