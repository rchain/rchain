package coop.rchain.rosette.prim
import scala.reflect.{classTag, ClassTag}
import coop.rchain.rosette.{Ctxt, Fixnum, Ob, RblBool, RblChar, RblString, Tuple}
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

      checkType[RblString](0, elem).flatMap { s1 =>
        checkType[RblString](1, elem) match {
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
        code <- checkType[Fixnum](0, elem)    // Ensure arg0 is a Fixnum
        sep  <- checkType[RblString](1, elem) // Ensure arg1 is a RblString
        tup  <- checkTupleString(2, elem)     // Ensure arg2 is a Tuple containing RblStrings
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

  /**
    * Define the string-set-nth primitive.
    * This replaces the n element of a String
    *
    * examples:
    *  (string-set-nth "abcdef" 2 #\C) ===> "abCdef"
    */
  object stringSetNth extends Prim {
    override val name: String = "string-set-nth"
    override val minArgs: Int = 3
    override val maxArgs: Int = 3

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.elem

      for {
        str <- checkType[RblString](0, elem)                // Ensure arg0 is a RblString
        n   <- checkFixnumBounds(1, elem, str.value.length) // Ensure arg1 is a Fixnum
        ch  <- checkType[RblChar](2, elem)                  // Ensure arg2 is a Char
      } yield {
        RblString(str.value.updated(n, ch.value).toString)
      }
    }
  }

  /**
    * Define the string-length primitive.
    * Returns the length of the string
    * e.g. (string-length "abcdefghi") ==> 9
    */
  object stringLength extends Prim {
    override val name: String = "string-length"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[RblString] // All args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val elem = ctxt.argvec.elem

      Right(Fixnum(elem(0).asInstanceOf[RblString].value.length))
    }
  }

  /**
    * Define the string-new primitive.
    * This creates a new RblString of length n. It is filled with
    * either spaces ' ' or an optionally specified character.
    *
    * examples:
    *  (string-new 5) ===> "     "
    *  (string-new 4 #\A) ===> "AAAA"
    */
  object stringNew extends Prim {
    override val name: String = "string-new"
    override val minArgs: Int = 1
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem  = ctxt.argvec.elem
      val nargs = ctxt.nargs

      for {
        n <- checkType[Fixnum](0, elem)
        res <- checkType[RblChar](1, elem) match {
                case Right(ch) => Right(RblString(ch.value.toString * n.value))
                case Left(_)   => Right(RblString(" " * n.value))
              }
      } yield res
    }
  }

  /**
    * Define the string-mem? primitive.
    * This returns #t if the specified character is in the string else #f
    *
    * examples:
    *  (string-mem? "abcdef" #\c) ===> #t
    *  (string-mem? "abcdef" #\z) ===> #f
    */
  object stringMemQ extends Prim {
    override val name: String = "string-mem?"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem

      for {
        str <- checkType[RblString](0, elem) // Ensure arg0 is a RblString
        ch  <- checkType[RblChar](1, elem)   // Ensure arg1 is a Char
      } yield {
        RblBool(str.value.contains(ch.value))
      }
    }
  }

  /** Helper functions begin here */
  /**
    * Check the parameter exists return IndexOutOfBounds if not.
    * Then check that it is the specified type. Return a PrimError if it is not
    * else return the parameter.
    */
  def checkType[T <: Ob](n: Int, elem: Seq[Ob])(implicit ct: ClassTag[T]): Either[PrimError, T] =
    if (n < 0 || n >= elem.size) {
      Left(IndexOutOfBounds(n, elem.size))
    } else {
      elem(n) match {
        case e: T => Right(elem(n).asInstanceOf[T])
        case _    => Left(TypeMismatch(n, ct.runtimeClass.getName))
      }
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
    * not a Fixnum or if it is out of bounds of 0 <= n <= size. Otherwise return the
    * value of the Fixnum as an Int.
    */
  private def checkFixnumBounds(n: Int, elem: Seq[Ob], size: Int): Either[PrimError, Int] =
    if (!elem(n).isInstanceOf[Fixnum]) {
      Left(TypeMismatch(n, Fixnum.getClass().getName()))
    } else {
      val i = elem(n).asInstanceOf[Fixnum].value
      if ((i < 0) || (i > size)) {
        Left(IndexOutOfBounds(i, size))
      } else {
        Right(i)
      }
    }
}
