package coop.rchain.roscala.prim
import coop.rchain.roscala.macros.checkTypeMismatch
import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.Prim._

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.reflect.ClassTag

object rblstring {

  /** Base RblString Binary operation routine.
    * It checks parameter types and returns a PrimError if the first parameter is not
    * an RblString, RblBool(false) if the second parameter is not an RblString, or the
    * result of the comparison function as an RblBool if both parameters are an RblString.
    */
  case class strBinOp(name: String, f: (String, String) => Boolean) extends Prim {
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.value

      checkType[RblString](0, elem).flatMap { s1 =>
        checkType[RblString](1, elem) match {
          case Right(s2) => Right(RblBool(f(s1.value, s2.value)))
          case Left(_)   => Right(RblBool(false))
        }
      }
    }
  }

  /** case sensitive */
  object stringEq   extends strBinOp("string=", _ == _)
  object stringNEq  extends strBinOp("string!=", _ != _)
  object stringLess extends strBinOp("string<", _ < _)
  object stringLEq  extends strBinOp("string<=", _ <= _)
  object stringGtr  extends strBinOp("string>", _ > _)
  object stringGEq  extends strBinOp("string>=", _ >= _)

  /** case insensitive */
  object stringCiEq   extends strBinOp("string-ci=", _.toLowerCase == _.toLowerCase)
  object stringCiNEq  extends strBinOp("string-ci!=", _.toLowerCase != _.toLowerCase)
  object stringCiLess extends strBinOp("string-ci<", _.toLowerCase < _.toLowerCase)
  object stringCiLEq  extends strBinOp("string-ci<=", _.toLowerCase <= _.toLowerCase)
  object stringCiGtr  extends strBinOp("string-ci>", _.toLowerCase > _.toLowerCase)
  object stringCiGEq  extends strBinOp("string-ci>=", _.toLowerCase >= _.toLowerCase)

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
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.value
      val init = ""

      Right(
        RblString(
          elem.foldLeft(init)((acc: String, el: Ob) => acc ++ el.asInstanceOf[RblString].value)
        )
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

    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.value

      for {
        code <- checkType[Fixnum](0, elem)    // Ensure arg0 is a Fixnum
        sep  <- checkType[RblString](1, elem) // Ensure arg1 is a RblString
        tup  <- checkTupleString(2, elem)     // Ensure arg2 is a Tuple containing RblStrings
      } yield {
        val strings = tup.value.foldLeft(List[String]()) { (list, ob) =>
          list :+ ob.asInstanceOf[RblString].value
        }

        def codeString(mask: Int) =
          if (((code.value & mask) != 0) && (tup.value.length > 0)) sep.value else ""

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

    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.value

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
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val elem = ctxt.argvec.value

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

    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.value

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

    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.value

      for {
        str <- checkType[RblString](0, elem) // Ensure arg0 is a RblString
        ch  <- checkType[RblChar](1, elem)   // Ensure arg1 is a Char
      } yield {
        RblBool(str.value.contains(ch.value))
      }
    }
  }

  /**
    * Define the string-get-token primitive.
    * This returns the nth substring delimited by one of the separators
    * in the second string.
    *
    * examples:
    * (string-get-token "aZ,bY.cX-dW=eV/fU*gT" 0 "/-=,.")   ====> "aZ"
    * (string-get-token "aZ,bY.cX-dW=eV/fU*gT" 6 "/-=,.")   ====> "gT"
    * (string-get-token "aZ,bY.cX-dW=eV/fU*gT" 8 "/-=,.")   ====> ""
    * (string-get-token "aZ,bY.cX-dW=eV/fU*gT" 5 "&")       ====> ""
    * (string-get-token "aZ,bY.cX-dW=eV/fU*gT" 3 "*")       ====> ""
    */
  object stringGetToken extends Prim {
    override val name: String = "string-get-token"
    override val minArgs: Int = 3
    override val maxArgs: Int = 3

    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblString] = {
      val elem = ctxt.argvec.value

      for {
        str <- checkType[RblString](0, elem) // Ensure arg0 is a RblString
        w   <- checkType[Fixnum](1, elem)    // Ensure arg1 is a Fixnum
        sep <- checkType[RblString](2, elem) // Ensure arg2 is a RblString
      } yield {
        val tokens = str.value.split(sep.value.toCharArray)
        if ((w.value >= 0) && (w.value < tokens.length))
          RblString(tokens(w.value))
        else
          RblString("")
      }
    }
  }

  /**
    * Define the string-split primitive.
    * This returns a Tuple containing tokens from a string.
    * Split is done at the specified separators.
    * An optional maximum number of splits may be specified
    * Leading and trailing separators are ignored.
    * if no speparators, return a Tuple of the string's characters ignoring the count
    *
    * examples:
    * (string-split "a,b,c,d,e,f" ",") ====> ["a" "b" "c" "d" "e" "f" #niv]
    * (string-split "a,b.c,d.e,f" "," 4) ====> ["a" "b.c" "d.e" "f" #niv]
    * (string-split "a,b.c,d.e,f" "," 0) ====> []
    * (string-split "a,b.c,d.e,f" "," -1) ==== []
    * (string-split ",a,b,c," "," 2) ====> ["a" "b" "c,"]
    * (string-split ",a,b,c," "," 1) ====> ["a" "b,c,"]
    * (string-split ",a,b,c," ",") ====> ["a" "b" "c" #niv]
    * (string-split "a,b,c" "") ====> [#\a #\, #\b #\, #\c]
    * (string-split "a,b,c,d,e,f" "," 3) ====> ["a" "b" "c" "d,e,f"]
    * (string-split "a,b.c,d.e,f" "," 2) ====> ["a" "b.c" "d.e,f"]
    * (string-split "a,b.c,d.e,f" ",." 2) ====> ["a" "b" "c,d.e,f"]
    * (string-split "a,b.c,d.e,f" "," 1) ====> ["a" "b.c,d.e,f"]
    */
  object stringSplit extends Prim {
    override val name: String = "string-split"
    override val minArgs: Int = 2
    override val maxArgs: Int = 3

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.value

      for {
        str <- checkType[RblString](0, elem) // Ensure arg0 is a RblString
        sep <- checkType[RblString](1, elem) // Ensure arg1 is a RblString
        w <- checkType[Fixnum](2, elem) // Ensure optional arg2 is a Fixnum
            match {
              case Right(v)                 => Right(v)
              case Left(TypeMismatch(_, _)) => Left(TypeMismatch(2, Fixnum.getClass.getName))
              case Left(_)                  => Right(Fixnum(Int.MaxValue))
            }
      } yield {

        // At this point:
        //  str = string to tokenize
        //  sep = string of separators
        //  w = count of tokens to return

        val tl = // Get the list of tokens
          if (sep.value.isEmpty) // Don't upset split with an empty list of separators
            Array.empty[String]
          else
            str.value.split(sep.value.toArray) // split ignores trailing separators

        val tr = tl.dropWhile(_.isEmpty) // Trim off leading separators

        if (sep.value.isEmpty) { // no separators? return a Tuple of RblChars
          Tuple(str.value.toCharArray.map(RblChar): _*)
        } else if (w.value <= 0) { // Invalid count yields an empty Tuple
          Nil
        } else if (w.value > tr.length) { // Consuming the whole string yields the tokens plus NIV
          Tuple(tr.map(RblString(_)) :+ Niv: _*)
        } else {
          // Handle counts less than available tokens.
          // This yields the tokens plus the remaining string

          val tokens = tr.take(w.value)                            // get the selected tokens
          val idx    = nthIndex(w.value, str.value, sep.value) + 1 // Index to the rest of the string
          val rest   = str.value.drop(idx)                         // The rest of the string

          Tuple(tokens.map(RblString(_)) :+ RblString(rest): _*)
        }
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
        case _: T => Right(elem(n).asInstanceOf[T])
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
      if (t.value.exists(el => !el.isInstanceOf[RblString])) {
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
      Left(TypeMismatch(n, Fixnum.getClass.getName))
    } else {
      val i = elem(n).asInstanceOf[Fixnum].value
      if ((i < 0) || (i > size)) {
        Left(IndexOutOfBounds(i, size))
      } else {
        Right(i)
      }
    }

  // Helper to find the index after the nth token.
  private def nthIndex(n: Int, str: String, sep: String): Int = {
    val sepSet: HashSet[Char] = HashSet.empty[Char] ++ sep

    @tailrec
    def work(chars: Iterator[(Char, Int)], count: Int): Int =
      if (chars.nonEmpty) {
        val (c, i)   = chars.next()
        val newCount = if (sepSet.contains(c)) count + 1 else count

        if (newCount == n) {
          i
        } else {
          work(chars, newCount)
        }
      } else {
        -1
      }

    work(str.iterator.zipWithIndex, 0)
  }
}
