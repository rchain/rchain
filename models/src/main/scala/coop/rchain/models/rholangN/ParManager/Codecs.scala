package coop.rchain.models.rholangN.ParManager

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import coop.rchain.models.rholangN.ParManager.Constants._
import coop.rchain.models.rholangN.ParManager.Sorting._
import coop.rchain.models.rholangN._

import java.io.{InputStream, OutputStream}

private[ParManager] object Codecs {
  def serialize(par: ParN, output: OutputStream): Unit = {
    val cos = CodedOutputStream.newInstance(output)

    def writeTag(x: Byte): Unit = cos.writeRawByte(x)

    def writeLength(x: Int): Unit = cos.writeUInt32NoTag(x)

    def writeInt(x: Int): Unit = cos.writeInt32NoTag(x)

    def writeLong(x: Long): Unit = cos.writeInt64NoTag(x)

    def writeBool(x: Boolean): Unit = cos.writeBoolNoTag(x)

    def writePars(ps: Seq[RhoTypeN]): Unit = ps.foreach(writePar)

    def writePar(p: RhoTypeN): Unit =
      p match {

        /** Main types */
        case parProc: ParProcN =>
          writeTag(PARPROC)
          writeLength(parProc.ps.size)
          writePars(sort(parProc.ps))

        case send: SendN =>
          writeTag(SEND)
          writePar(send.chan)
          writeLength(send.data.size)
          writePars(send.data)
          writeBool(send.persistent)

        /** Ground types */
        case _: GNilN =>
          writeTag(GNIL)

        case gInt: GIntN =>
          writeTag(GINT)
          writeLong(gInt.v)

        /** Collections */
        case eList: EListN =>
          writeTag(ELIST)
          writeLength(eList.ps.size)
          writePars(eList.ps)

        /** Vars */
        case bVar: BoundVar =>
          writeTag(BOUND_VAR)
          writeInt(bVar.value)

        case fVar: FreeVar =>
          writeTag(FREE_VAR)
          writeInt(fVar.value)

        case _: Wildcard =>
          writeTag(WILDCARD)

        /** Expr */
        /** Bundle */
        /** Connective */
        case _ => assert(assertion = false, "Not defined type")

      }

    writePar(par)
    cos.flush()
  }

  def deserialize(input: InputStream): ParN = {
    val cis = CodedInputStream.newInstance(input)

    def readTag(): Byte = cis.readRawByte()

    def readLength(): Int = cis.readUInt32()

    def readInt(): Int = cis.readInt32()

    def readLong(): Long = cis.readInt64()

    def readBool(): Boolean = cis.readBool()

    def readPars(count: Int): Seq[ParN] = (1 to count).map(_ => readPar())

    def readPar(): ParN = {
      val tag = readTag()
      tag match {

        /** Main types */
        case PARPROC =>
          val count = readLength()
          val ps    = readPars(count)
          ParProcN(ps)

        case SEND =>
          val chan       = readPar()
          val dataSize   = readLength()
          val dataSeq    = readPars(dataSize)
          val persistent = readBool()
          SendN(chan, dataSeq, persistent)

        /** Ground types */
        case GNIL =>
          GNilN()

        case GINT =>
          val v = readLong()
          GIntN(v)

        /** Collections */
        case ELIST =>
          val count = readLength()
          val ps    = readPars(count)
          EListN(ps)

        /** Vars */
        case BOUND_VAR =>
          val v = readInt()
          BoundVar(v)

        case FREE_VAR =>
          val v = readInt()
          FreeVar(v)

        case WILDCARD =>
          Wildcard()

        /** Expr */
        /** Bundle */
        /** Connective */
        case _ =>
          assert(assertion = false, "Invalid tag for ParN deserialization")
          GNilN()
      }
    }
    readPar()
  }
}
