package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.models.ParSet
import coop.rchain.models.ParMap
import Expr._

/** Any process may be an operand to an expression.
  * Only processes equivalent to a ground process of compatible type will reduce.
  */
final case class Expr(
    exprInstance: ExprInstance = ExprInstance.Empty
)

object Expr {
  sealed trait ExprInstance {
    def gBool: Option[Boolean]                       = None
    def gInt: Option[Long]                           = None
    def gString: Option[String]                      = None
    def gUri: Option[String]                         = None
    def gByteArray: Option[ByteString]               = None
    def eNotBody: Option[ENot]                       = None
    def eNegBody: Option[ENeg]                       = None
    def eMultBody: Option[EMult]                     = None
    def eDivBody: Option[EDiv]                       = None
    def ePlusBody: Option[EPlus]                     = None
    def eMinusBody: Option[EMinus]                   = None
    def eLtBody: Option[ELt]                         = None
    def eLteBody: Option[ELte]                       = None
    def eGtBody: Option[EGt]                         = None
    def eGteBody: Option[EGte]                       = None
    def eEqBody: Option[EEq]                         = None
    def eNeqBody: Option[ENeq]                       = None
    def eAndBody: Option[EAnd]                       = None
    def eOrBody: Option[EOr]                         = None
    def eVarBody: Option[EVar]                       = None
    def eListBody: Option[EList]                     = None
    def eTupleBody: Option[ETuple]                   = None
    def eSetBody: Option[ParSet]                     = None
    def eMapBody: Option[ParMap]                     = None
    def eMethodBody: Option[EMethod]                 = None
    def eMatchesBody: Option[EMatches]               = None
    def ePercentPercentBody: Option[EPercentPercent] = None
    def ePlusPlusBody: Option[EPlusPlus]             = None
    def eMinusMinusBody: Option[EMinusMinus]         = None
    def eModBody: Option[EMod]                       = None
  }
  object ExprInstance {
    case object Empty                                            extends ExprInstance
    final case class GBool(value: Boolean)                       extends ExprInstance
    final case class GInt(value: Long)                           extends ExprInstance
    final case class GString(value: String)                      extends ExprInstance
    final case class GUri(value: String)                         extends ExprInstance
    final case class GByteArray(value: ByteString)               extends ExprInstance
    final case class ENotBody(value: ENot)                       extends ExprInstance
    final case class ENegBody(value: ENeg)                       extends ExprInstance
    final case class EMultBody(value: EMult)                     extends ExprInstance
    final case class EDivBody(value: EDiv)                       extends ExprInstance
    final case class EPlusBody(value: EPlus)                     extends ExprInstance
    final case class EMinusBody(value: EMinus)                   extends ExprInstance
    final case class ELtBody(value: ELt)                         extends ExprInstance
    final case class ELteBody(value: ELte)                       extends ExprInstance
    final case class EGtBody(value: EGt)                         extends ExprInstance
    final case class EGteBody(value: EGte)                       extends ExprInstance
    final case class EEqBody(value: EEq)                         extends ExprInstance
    final case class ENeqBody(value: ENeq)                       extends ExprInstance
    final case class EAndBody(value: EAnd)                       extends ExprInstance
    final case class EOrBody(value: EOr)                         extends ExprInstance
    final case class EVarBody(value: EVar)                       extends ExprInstance
    final case class EListBody(value: EList)                     extends ExprInstance
    final case class ETupleBody(value: ETuple)                   extends ExprInstance
    final case class ESetBody(value: ParSet)                     extends ExprInstance
    final case class EMapBody(value: ParMap)                     extends ExprInstance
    final case class EMethodBody(value: EMethod)                 extends ExprInstance
    final case class EMatchesBody(value: EMatches)               extends ExprInstance
    final case class EPercentPercentBody(value: EPercentPercent) extends ExprInstance
    final case class EPlusPlusBody(value: EPlusPlus)             extends ExprInstance
    final case class EMinusMinusBody(value: EMinusMinus)         extends ExprInstance
    final case class EModBody(value: EMod)                       extends ExprInstance
  }
}
