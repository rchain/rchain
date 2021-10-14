package coop.rchain.rholang.interpreter.merging

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.ListParWithRandom
import coop.rchain.rholang.interpreter.RhoType.Number
import coop.rchain.rholang.interpreter.storage
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.merger.ChannelChange
import coop.rchain.rspace.serializers.ScodecSerialize
import coop.rchain.rspace.trace.Produce
import coop.rchain.rspace.{HotStoreTrieAction, TrieInsertBinaryProduce}
import coop.rchain.scodec.codecs
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bytes, int64, uint16, variableSizeBytes}

object RholangMergingLogic {

  /**
    * Transforms absolute values with the difference from initial values.
    *
    * E.g. for 3 state changes, A, B, C are channels, PSH is initial value (pre-state hash).
    *
    *        A   B   C        A   B   C
    *  ---------------       ----------
    *  PSH  10   2  20
    *
    *   0.  20               10
    *   1.       5      ==>       3
    *   2.  15      10       -5     -10
    *
    * @param channelValues Final values
    * @param getInitialValue Accessor to initial value
    */
  def calculateNumChannelDiff[F[_]: Concurrent, Key](
      channelValues: Seq[Map[Key, Long]],
      getInitialValue: Key => F[Option[Long]]
  ): F[List[Map[Key, Long]]] = {
    val initValuesF = channelValues
      .flatMap(_.keySet)
      .distinct
      .toList
      // Read initial value, default is 0 if not found
      .traverse(key => getInitialValue(key).map(v => (key, v.getOrElse(0L))))
      .map(_.toMap)
      .flatMap(Ref.of(_))

    initValuesF >>= { st =>
      channelValues.toList.traverse { endValMap =>
        endValMap.toList
          .traverse {
            case (ch, endVal) =>
              for {
                prevValMap <- st.get
                prevValOpt = prevValMap.get(ch)

                // Calculate diff for initialized values
                diffOpt <- prevValOpt
                            .map { prevVal =>
                              // Calculate difference from previous value
                              val diff = endVal - prevVal

                              // Update current final value / return diff
                              val newLastEndValMap = prevValMap + ((ch, endVal))
                              st.set(newLastEndValMap).as(diff.some)
                            }
                            .getOrElse(none.pure[F])
              } yield diffOpt.map((ch, _))
          }
          .map(_.flatten.toMap)
      }
    }
  }

  /**
    * Merge number channel value from multiple changes and base state.
    *
    * @param channelHash Channel hash
    * @param diff Difference from base state
    * @param changes Channel changes to calculate new random generator
    * @param getBaseData Base state value reader
    */
  def calculateNumberChannelMerge[F[_]: Monad](
      channelHash: Blake2b256Hash,
      diff: Long,
      changes: ChannelChange[ByteVector],
      getBaseData: Blake2b256Hash => F[Seq[Datum[ListParWithRandom]]]
  ): F[HotStoreTrieAction] =
    for {
      // Read initial value of number channel from base state
      initValOpt <- convertToReadNumber(getBaseData).apply(channelHash)

      // Calculate number channel new value
      initNum = initValOpt getOrElse 0L
      newVal  = initNum + diff

      // Calculate merged random generator
      newRnd = if (changes.added.size == 1) {
        // Single branch, just use available random generator
        decodeRnd(changes.added.head)
      } else {
        // Multiple branches, merge random generators
        val rndAddedSorted = changes.added
          .map(decodeRnd)
          .distinct
          .map(rnd => rnd -> Blake2b512Random.typeMapper.toBase(rnd))
          .sortBy(_._2)(byteStringOrdering)
          .map(_._1)

        Blake2b512Random.merge(rndAddedSorted)
      }

      // Create final merged value
      datumEncoded = createDatumEncoded(channelHash, newVal, newRnd)

      // Create update store action
      storeAction = TrieInsertBinaryProduce(channelHash, Seq(datumEncoded))
    } yield storeAction

  private val byteStringOrdering =
    Ordering.by[ByteString, Iterable[Byte]](_.toByteArray)(Ordering.Iterable[Byte])

  /* Number channel value encoders/decoders */

  def decodeRnd(parWithRndEncoded: ByteVector): Blake2b512Random = {
    val parsWithRnd = codecPars.decode(parWithRndEncoded.bits).require.value

    parsWithRnd.randomState
  }

  def getNumberWithRnd(parWithRnd: ListParWithRandom): (Long, Blake2b512Random) = {
    assert(parWithRnd.pars.size == 1, {
      s"Number channel should contain single Int term, found ${parWithRnd.pars}."
    })

    val Number(num) = parWithRnd.pars.head

    (num, parWithRnd.randomState)
  }

  def createDatumEncoded(
      channelHash: Blake2b256Hash,
      num: Long,
      rnd: Blake2b512Random
  ): ByteVector = {
    // Create value with random generator
    val numPar     = Number(num)
    val parWithRnd = ListParWithRandom(Seq(numPar), rnd)
    // Create hash of the data
    val dataHash = StableHashProvider.hash(channelHash.bytes, parWithRnd, persist = false)(
      storage.serializePars
    )
    // Create produce
    val produce = Produce(channelHash, dataHash, persistent = false)
    // Create datum
    val datum = Datum(parWithRnd, persist = false, produce)
    // Encode datum
    ScodecSerialize.encodeDatum(datum)(storage.serializePars)
  }

  private val codecPars = storage.serializePars.toSizeHeadCodec

  /**
    * Converts function to get all data on a channel to function to get single number value.
    */
  def convertToReadNumber[F[_]: Monad](
      getDataFunc: Blake2b256Hash => F[Seq[Datum[ListParWithRandom]]]
  ): Blake2b256Hash => F[Option[Long]] =
    (hash: Blake2b256Hash) =>
      // Read existing value
      getDataFunc(hash).map { data =>
        assert(data.size <= 1, {
          s"To calculate difference on a number channel, single value is expected, found $data."
        })

        // Extract single number value if exists
        data.headOption.map(_.a).map(getNumberWithRnd).map(_._1)
      }

  /* Mergeable channel store encoders/decoders */

  final case class DeployMergeableData(channels: Seq[NumberChannel])

  final case class NumberChannel(hash: Blake2b256Hash, diff: Long)

  val codecBlake2b256Hash: Codec[Blake2b256Hash] =
    scodec.codecs
      .bytes(size = Blake2b256Hash.length)
      .xmap[Blake2b256Hash](Blake2b256Hash.fromByteVector, _.bytes)
      .as[Blake2b256Hash]

  val numberChannelCodec: Codec[NumberChannel] =
    (codecBlake2b256Hash :: int64).as[NumberChannel]

  // Deploy mergeable channels support from Replay
  val deployMergeableDataCodec: Codec[DeployMergeableData] =
    codecs.seqOfN(uint16, numberChannelCodec).as[DeployMergeableData]

  val deployMergeableDataSeqCodec: Codec[Seq[DeployMergeableData]] =
    codecs.seqOfN(uint16, deployMergeableDataCodec).as[Seq[DeployMergeableData]]

  val codecByteVectorVar: Codec[ByteVector] = variableSizeBytes(uint16, bytes)

  val codecMergeableKey: Codec[(ByteVector, ByteVector, Int)] =
    (codecByteVectorVar :: codecByteVectorVar :: uint16).as[(ByteVector, ByteVector, Int)]

}
