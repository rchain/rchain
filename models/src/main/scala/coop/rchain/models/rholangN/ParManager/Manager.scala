package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._
import scodec.bits.ByteVector

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

object Manager {

  def parToBytes(p: RhoTypeN): ByteVector = {
    val baos = new ByteArrayOutputStream(p.serializedSize)
    Codecs.serialize(p, baos)
    ByteVector(baos.toByteArray)
  }

  def parFromBytes(bv: ByteVector): RhoTypeN = {
    val bais = new ByteArrayInputStream(bv.toArray)
    Codecs.deserialize(bais)
  }

  def equals(self: RhoTypeN, other: Any): Boolean = other match {
    case x: RhoTypeN => x.rhoHash == self.rhoHash
    case _           => false
  }

  /** Main types */
  def createParProc(ps: Seq[ParN]): ParProcN = Constructor.createParProc(ps)
  def createSend(chan: ParN, data: Seq[ParN], persistent: Boolean): SendN = Constructor.createSend(chan, data, persistent)

  /** Ground types */
  def createGNil: GNilN = Constructor.createGNil

  def createGInt(v: Long): GIntN = Constructor.createGInt(v)

  /** Collections */
  def createEList(ps: Seq[ParN], remainder: Option[VarN]): EListN = Constructor.createEList(ps, remainder)

  /** Vars */
  def createBoundVar(value: Int): BoundVar = Constructor.createBoundVar(value)

  def createFreeVar(value: Int): FreeVar = Constructor.createFreeVar(value)

  def createWildcard: Wildcard = Constructor.createWildcard

  /** Expr */

  /** Bundle */

  /** Connective */
}
