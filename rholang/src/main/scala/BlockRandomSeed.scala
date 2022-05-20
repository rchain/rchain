import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b256.hash
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector
import scodec.{Codec, TransformSyntax}
import scodec.codecs.{bytes, uint16, uint8, ulong, utf8, variableSizeBytes}

final case class BlockRandomSeed(
    shardId: String,
    blockNumber: Long,
    sender: PublicKey,
    validatorPublicKey: PublicKey,
    preStateHash: Blake2b256Hash
)

object BlockRandomSeed {
  private val codecPublicKey: Codec[PublicKey] = variableSizeBytes(uint8, bytes)
    .xmap[PublicKey](bv => PublicKey(bv.toArray), pk => ByteVector(pk.bytes))
  private val codecBlakeHash: Codec[Blake2b256Hash] =
    variableSizeBytes(uint16, bytes)
      .xmap[Blake2b256Hash](bv => Blake2b256Hash.fromByteVector(bv), _.bytes)

  private val codecBlockRandomSeed: Codec[BlockRandomSeed] = (utf8 :: ulong(bits = 64) ::
    codecPublicKey :: codecPublicKey :: codecBlakeHash).as[BlockRandomSeed]

  private def encode(blockRandomSeed: BlockRandomSeed): Array[Byte] =
    codecBlockRandomSeed.encode(blockRandomSeed).require.toByteArray

  def randomSeed(blockRandomSeed: BlockRandomSeed): Blake2b512Random = {
    val serialized = encode(blockRandomSeed)
    Blake2b512Random(serialized)
  }
}
