package coop.rchain.models.rholangN.ParManager

import com.google.protobuf.CodedOutputStream
import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object SerializedSize {

  import Constants._

  private def sizeTag(): Int = tagSize

  private def sizeLength(value: Int): Int = CodedOutputStream.computeUInt32SizeNoTag(value)

  private def sizeInt(value: Int): Int = CodedOutputStream.computeInt32SizeNoTag(value)

  private def sizeLong(value: Long): Int = CodedOutputStream.computeInt64SizeNoTag(value)

  private def sizeBool(): Int = 1

  private def sizePar(p: ParN): Int = p.serializedSize

  private def sizePars(ps: Seq[ParN]): Int = ps.map(sizePar).sum

  def serializedSizeFn(p: RhoTypeN): Int = p match {

    /** Main types */
    case pproc: ParProcN =>
      val tagSize    = sizeTag()
      val lengthSize = sizeLength(pproc.ps.size)
      val psSize     = sizePars(pproc.ps)
      tagSize + lengthSize + psSize

    case send: SendN =>
      val tagSize        = sizeTag()
      val chanSize       = sizePar(send.chan)
      val dataLengthSize = sizeLength(send.data.size)
      val dataSize       = sizePars(send.data)
      val persistentSize = sizeBool()
      tagSize + chanSize + dataLengthSize + dataSize + persistentSize

    /** Ground types */
    case _: GNilN    => sizeTag()
    case gInt: GIntN => sizeTag() + sizeLong(gInt.v)

    /** Collections */
    case list: EListN =>
      val tagSize    = sizeTag()
      val lengthSize = sizeLength(list.ps.size)
      val psSize     = sizePars(list.ps)
      tagSize + lengthSize + psSize

    /** Vars */
    case v: BoundVar => sizeTag() + sizeInt(v.value)
    case v: FreeVar  => sizeTag() + sizeInt(v.value)
    case _: Wildcard => sizeTag()

    /** Expr */
    /** Bundle */
    /** Connective */
    case _ =>
      assert(assertion = false, "Not defined type")
      0
  }
}
