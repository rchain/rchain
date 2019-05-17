package coop.rchain.rspace
package nextgenrspace

import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.internal._
import coop.rchain.rspace.nextgenrspace.history.{HistoryAction, PersistedData}
import scodec.Codec

object Measure {

  def create[C, P, A, K]()(
      implicit
      codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ): Measure[C, P, A, K] =
    new Measure[C, P, A, K]()(codecC, codecP, codecA, codecK): Measure[C, P, A, K]

}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class Measure[C, P, A, K](
    implicit
    codecC: Codec[C],
    codecP: Codec[P],
    codecA: Codec[A],
    codecK: Codec[K]
) {

  protected[this] val dataLogger: Logger = Logger("coop.rchain.rspace.datametrics")

  protected def measure(value: TrieUpdate[C, P, A, K]): Unit =
    dataLogger.whenDebugEnabled {
      val maybeData = value match {
        case _ @TrieUpdate(_, operation, channelsHash, gnat) =>
          val hex     = channelsHash.bytes.toHex
          val data    = gnat.data
          val dataLen = Codec[Seq[Datum[A]]].encode(data).get.size
          val wks     = gnat.wks
          val wksLen  = Codec[Seq[WaitingContinuation[P, K]]].encode(wks).get.size
          val gnatLen = Codec[GNAT[C, P, A, K]].encode(gnat).get.size
          Some((hex, gnatLen, operation.toString, data.size, dataLen, wks.size, wksLen))
        case _ => None
      }
      maybeData.foreach {
        case (key, size, action, datumSize, datumLen, continuationSize, continuationLen) =>
          dataLogger.debug(
            s"$key;$size;$action;$datumSize;$datumLen;$continuationLen;$continuationSize"
          )
      }
    }
}
