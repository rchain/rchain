package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN.ParManager.RhoHash._
import coop.rchain.models.rholangN.ParManager.SerializedSize._
import coop.rchain.models.rholangN.ParManager.SubstituteRequired._
import coop.rchain.models.rholangN.ParManager.ConnectiveUsed._
import coop.rchain.models.rholangN.ParManager.LocallyFree._
import coop.rchain.models.rholangN.ParManager.EvalRequired._
import coop.rchain.models.rholangN._

private[ParManager] object Constructor {

  /** Main types */
  def createParProc(ps: Seq[ParN]): ParProcN = {
    val meta = new ParMetaData(
      () => hashParProc(ps),
      () => sizeParProc(ps),
      () => locallyFreeParProc(ps),
      () => connectiveUsedParProc(ps),
      () => evalRequiredParProc(ps),
      () => substituteRequiredParProc(ps)
    )
    new ParProcN(ps, meta)
  }

  def createSend(chan: ParN, data: Seq[ParN], persistent: Boolean): SendN = {
    val meta = new ParMetaData(
      () => hashSend(chan, data, persistent),
      () => sizeSend(chan, data, persistent),
      () => locallyFreeSend(chan, data, persistent),
      () => connectiveUsedSend(chan, data, persistent),
      () => evalRequiredSend(chan, data, persistent),
      () => substituteRequiredSend(chan, data, persistent)
    )
    new SendN(chan, data, persistent, meta)
  }

  /** Ground types */
  def createGNil: GNilN = {
    val meta = new ParMetaData(
      () => hashGNil(),
      () => sizeGNil(),
      () => locallyFreeGNil(),
      () => connectiveUsedGNil(),
      () => evalRequiredGNil(),
      () => substituteRequiredGNil()
    )
    new GNilN(meta)
  }

  def createGInt(v: Long): GIntN = {
    val meta = new ParMetaData(
      () => hashGInt(v),
      () => sizeGInt(v),
      () => locallyFreeGInt(v),
      () => connectiveUsedGInt(v),
      () => evalRequiredGInt(v),
      () => substituteRequiredGInt(v)
    )
    new GIntN(v, meta)
  }

  /** Collections */
  def createEList(ps: Seq[ParN], remainder: Option[VarN]): EListN = {
    val meta = new ParMetaData(
      // TODO: Add remainder to all functions
      () => hashEList(ps),
      () => sizeEList(ps),
      () => locallyFreeEList(ps),
      () => connectiveUsedEList(ps),
      () => evalRequiredEList(ps),
      () => substituteRequiredEList(ps)
    )
    new EListN(ps, remainder, meta)
  }

  /** Vars */
  def createBoundVar(value: Int): BoundVar = {
    val meta = new ParMetaData(
      () => hashBoundVar(value),
      () => sizeBoundVar(value),
      () => locallyFreeBoundVar(value),
      () => connectiveUsedBoundVar(value),
      () => evalRequiredBoundVar(value),
      () => substituteRequiredBoundVar(value)
    )
    new BoundVar(value, meta)
  }

  def createFreeVar(value: Int): FreeVar = {
    val meta = new ParMetaData(
      () => hashFreeVar(value),
      () => sizeFreeVar(value),
      () => locallyFreeFreeVar(value),
      () => connectiveUsedFreeVar(value),
      () => evalRequiredFreeVar(value),
      () => substituteRequiredFreeVar(value)
    )
    new FreeVar(value, meta)
  }

  def createWildcard: Wildcard = {
    val meta = new ParMetaData(
      () => hashWildcard(),
      () => sizeWildcard(),
      () => locallyFreeWildcard(),
      () => connectiveUsedWildcard(),
      () => evalRequiredWildcard(),
      () => substituteRequiredWildcard()
    )
    new Wildcard(meta)
  }

  /** Expr */

  /** Bundle */

  /** Connective */
}
