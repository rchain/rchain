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

  /** Main types */
  def sizeParProc(ps: Seq[ParN]): Int = {
    val tagSize = sizeTag()
    val lengthSize = sizeLength(ps.size)
    val psSize = sizePars(ps)
    tagSize + lengthSize + psSize
  }

  def sizeSend(chan: ParN, data: Seq[ParN], @unused persistent: Boolean): Int = {
    val tagSize = sizeTag()
    val chanSize = sizePar(chan)
    val dataLengthSize = sizeLength(data.size)
    val dataSize = sizePars(data)
    val persistentSize = sizeBool()
    tagSize + chanSize + dataLengthSize + dataSize + persistentSize
  }

  /** Ground types */
  def sizeGNil(): Int = sizeTag()

  def sizeGInt(v: Long): Int = sizeTag() + sizeLong(v)

  /** Collections */
  def sizeEList(ps: Seq[ParN]): Int = {
    val tagSize = sizeTag()
    val lengthSize = sizeLength(ps.size)
    val psSize = sizePars(ps)
    tagSize + lengthSize + psSize
  }

  /** Vars */
  def sizeBoundVar(value: Int): Int = sizeTag() + sizeInt(value)

  def sizeFreeVar(value: Int): Int = sizeTag() + sizeInt(value)

  def sizeWildcard(): Int = sizeTag()

  /** Expr */

  /** Bundle */

  /** Connective */
}
