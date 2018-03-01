package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Fixnum, Ob, RblBool, RblString, Tuple}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

object rblstring {

  /** Base RblString Binary operation routine.
    * It checks parameter types and returns a PrimError if the first parameter is not
    * an RblString, RblBool(false) if the second parameter is not an RblString, or the
    * result of the comparison function as an RblBool if both parameters are an RblString.
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
          case Left(_)   => Right(RblBool(false))
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

  /** Manipulation */
  /**
    * Define the string-concat primitive.
    * This concatenates n Strings
    * e.g. (string-concat "abc" "de" "fghi") ==> "abcdefghi"
    */
  object stringConcat extends Prim {
    override val name: String = "string-concat"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[RblString] // All args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.elem
      val n    = ctxt.nargs
      val init = ""

      Right(
        RblString(
          elem.foldLeft(init)((acc: String, el: Ob) => (acc ++ el.asInstanceOf[RblString].value)))
      )
    }
  }

  /**
    * Define the string-join primitive.
    * This concatenates n Strings from a Tuple with a separator. The placement of the seperator is controlled
    * by a numeric code (actually a bit-field. 0x1 == separator before, 0x2 == separator after).
    *
    * examples:
    *  (string-join 0 ":" ["123" "456" "789"]) ===> "123:456:789"
    *  (string-join 1 ":" ["123" "456" "789"]) ===> ":123:456:789"
    *  (string-join 2 ":" ["123" "456" "789"]) ===> "123:456:789:"
    *  (string-join 3 ":" ["123" "456" "789"]) ===> ":123:456:789:"
    *  (string-join 3 ":" []) ===> ""
    */
  object stringJoin extends Prim {
    override val name: String = "string-join"
    override val minArgs: Int = 3
    override val maxArgs: Int = 3

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.elem

      for {
        code <- checkFixnum(0, elem)      // Ensure arg0 is a Fixnum
        sep  <- checkRblString(1, elem)   // Ensure arg1 is a RblString
        tup  <- checkTupleString(2, elem) // Ensure arg2 is a Tuple containing RblStrings
      } yield {
        val strings = tup.elem.foldLeft(List[String]()) { (list, ob) =>
          list :+ ob.asInstanceOf[RblString].value
        }

        def codeString(mask: Int) =
          (if (((code.value & mask) != 0) && (tup.elem.size > 0)) sep.value else "")

        RblString(codeString(0x1) + strings.mkString(sep.value) + codeString(0x2))
      }
    }
  }

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

  /**
    * Check the specified parameter for type Tuple and contains all RblSTrings.
    * Return a PrimError if it is not else return the Tuple.
    */
  private def checkTupleString(n: Int, elem: Seq[Ob]): Either[PrimError, Tuple] =
    if (!elem(n).isInstanceOf[Tuple]) {
      Left(TypeMismatch(n, Tuple.getClass.getName))
    } else {
      val t = elem(n).asInstanceOf[Tuple]
      if (t.elem.exists(el => (!el.isInstanceOf[RblString]))) {
        Left(TypeMismatch(n, RblString.getClass.getName))
      } else {
        Right(t)
      }

    }

  /**
    * Check the specified parameter for type Fixnum. Return a PrimError if it is
    * not else return the Fixnum.
    */
  private def checkFixnum(n: Int, elem: Seq[Ob]): Either[PrimError, Fixnum] =
    if (!elem(n).isInstanceOf[Fixnum]) {
      Left(TypeMismatch(n, Fixnum.getClass().getName()))
    } else {
      Right(elem(n).asInstanceOf[Fixnum])
    }

  /**
    * Check the specified parameter for type RblString. Return a PrimError if it is
    * not else return the RblString.
    */
  private def checkRblString(n: Int, elem: Seq[Ob]): Either[PrimError, RblString] =
    if (!elem(n).isInstanceOf[RblString]) {
      Left(TypeMismatch(n, RblString.getClass.getName))
    } else {
      Right(elem(n).asInstanceOf[RblString])
    }

}
