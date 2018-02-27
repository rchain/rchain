package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Fixnum, Ob, RblBool, RblString}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

object rblstring {

  /** Base RblString Binary operation routine.
    * It checks parameter types and returns a PrimError if the first parameter is not
    * an RblString, RblBool(false) if the second parameter is not an RblString, or the
    * result of the comparison as an RblBool if both parameters are an RblString.
    */
  case class strBinOp(name: String, f: (String, String) => Boolean) extends Prim {
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem

      checkRblStringFirst(elem(0)).flatMap { s1 =>
        checkRblStringSecond(elem(1)) match {
          case Right(s2) => Right(RblBool(f(s1.value, s2.value)))
          case Left(res) => Right(res)
        }
      }
    }
  }

  /** case sensitive */
  object stringEq   extends strBinOp("string=", (_ == _))
  object stringNEq  extends strBinOp("string!=", (_ != _))
  object stringLess extends strBinOp("string<", (_ < _))
  object stringLEq  extends strBinOp("string<=", (_ <= _))
  object stringGtr  extends strBinOp("string>", (_ > _))
  object stringGEq  extends strBinOp("string>=", (_ >= _))

  /** case insensitive */
  object stringCiEq   extends strBinOp("string-ci=", (_.toLowerCase == _.toLowerCase))
  object stringCiNEq  extends strBinOp("string-ci!=", (_.toLowerCase != _.toLowerCase))
  object stringCiLess extends strBinOp("string-ci<", (_.toLowerCase < _.toLowerCase))
  object stringCiLEq  extends strBinOp("string-ci<=", (_.toLowerCase <= _.toLowerCase))
  object stringCiGtr  extends strBinOp("string-ci>", (_.toLowerCase > _.toLowerCase))
  object stringCiGEq  extends strBinOp("string-ci>=", (_.toLowerCase >= _.toLowerCase))

  /** Helper functions begin here */
  /**
    * Check the parameter for type RblString. Return a PrimError if it is
    * not else return the RblString.
    */
  private def checkRblStringFirst(parm: Ob): Either[PrimError, RblString] =
    if (!parm.isInstanceOf[RblString]) {
      Left(TypeMismatch(0, RblString.getClass.getName))
    } else {
      Right(parm.asInstanceOf[RblString])
    }

  /**
    * Check the specified parameter for type RblString. Return RblBool(false) if it is
    * not else return the RblString.
    */
  private def checkRblStringSecond(parm: Ob): Either[RblBool, RblString] =
    if (!parm.isInstanceOf[RblString]) {
      Left(RblBool(false))
    } else {
      Right(parm.asInstanceOf[RblString])
    }

}
