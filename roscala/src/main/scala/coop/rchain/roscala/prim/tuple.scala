package coop.rchain.roscala.prim

import coop.rchain.roscala.ob.{Ctxt, Fixnum, Ob, RblBool, Tuple}
import coop.rchain.roscala.macros.{checkTypeMismatch}
import coop.rchain.roscala.prim.Prim._
import coop.rchain.roscala.ob.Nil

object tuple {

  /**
    * Define the tuple-cons primitive.
    * This prepends an Ob to the specified Tuple
    * e.g. (tuple-cons 1 [2 3]) ==> [1 2 3]
    */
  object tplCons extends Prim {
    override val name: String = "tuple-cons"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value = ctxt.argvec.value
      checkTuple(1, value).map(Tuple.cons(value.head, _)) // arg1 must be a Tuple
    }
  }

  /**
    * Define the tuple-cons* primitive.
    * This prepends n Obs to the specified Tuple
    * e.g. (tuple-cons* 1 2 3 4 [5 6]) ==> [1 2 3 4 5 6]
    */
  object tplConsStar extends Prim {
    override val name: String = "tuple-cons*"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value = ctxt.argvec.value
      val nargs = ctxt.nargs
      val last  = nargs - 1
      val slice = value.slice(0, last)

      checkTuple(last, value)
        .map(Tuple(Tuple(slice), _)) // last arg must be a Tuple
    }
  }

  /**
    * Define the "tuple-rcons" primitive.
    * This appends an Ob to the specified Tuple
    * e.g. (tuple-rcons [1 2] 3) ==> [1 2 3]
    */
  object tplRcons extends Prim {
    override val name: String = "tuple-rcons"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value = ctxt.argvec.value

      checkTuple(0, value).map(Tuple.rcons(_, value(1))) // arg0 must be a Tuple
    }
  }

  /**
    * Define the tuple-concat primitive.
    * This concatenates n Tuples
    * e.g. (tuple-concat [1 2 3] [4] [5 6]) ==> [1 2 3 4 5 6]
    */
  object tplConcat extends Prim {
    override val name: String = "tuple-concat"
    override val minArgs: Int = 0
    override val maxArgs: Int = MaxArgs

    @checkTypeMismatch[Tuple] // All args must be Tuples
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value       = ctxt.argvec.value
      val init: Tuple = Nil

      Right(
        value.foldLeft(init)(
          (acc: Tuple, el: Ob) => Tuple(acc, el.asInstanceOf[Tuple])
        )
      )
    }
  }

  /**
    * Define the "tuple-safe-nth" primitive.
    * This safely returns the nth valueent of the specified Tuple
    * e.g. (tuple-safe-nth [1 2 3 4 5] 3) ==> 4
    */
  object tplSafeNth extends Prim {
    override val name: String = "tuple-safe-nth"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Ob] = {
      val value = ctxt.argvec.value

      checkTuple(0, value).flatMap( // Ensure arg0 is a Tuple
        t =>
          checkFixnum(1, value).map( // Ensure arg1 is a Fixnum
            n =>
              if (n.value < 0)
                Fixnum(Int.MinValue)
              else if (n.value < t.value.length) {
                t.nthLift(n.value) match {
                  case Some(v: Ob) => v
                  case None        => Fixnum(Int.MaxValue)
                }
              } else
                Fixnum(Int.MaxValue)
          )
      )
    }
  }

  /**
    * Define the "tuple-xchg" primitive.
    * This exchanges the nth and mth valueents of the specified Tuple
    * e.g. (tuple-xchg [1 2 3 4 5] 1 3) ==> [1 4 3 2 5]
    */
  object tplXchg extends Prim {
    override val name: String = "tuple-xchg"
    override val minArgs: Int = 3
    override val maxArgs: Int = 3

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value = ctxt.argvec.value

      def wrapper[A, B](f: Int => Option[Ob], v: Int, size: Int): Either[PrimError, Ob] =
        f(v).toRight(IndexOutOfBounds(v, size))

      // Check and get arguments: Tuple, Fixnum, Fixnum
      checkTuple(0, value).flatMap(
        t => // Ensure arg0 is a Tuple
          checkFixnum(1, value).flatMap(
            n => // Ensure arg1 is a Fixnum
              checkFixnum(2, value).flatMap { m => // Ensure arg2 is a Fixnum

                val f = wrapper(_: Int => Option[Ob], _: Int, t.value.length)
                for {
                  nv  <- f(t.nthLift, n.value)
                  mv  <- f(t.nthLift, m.value)
                  t1  <- f(t.setNthLift(_, mv), n.value)
                  res <- f(t1.asInstanceOf[Tuple].setNthLift(_, nv), m.value)
                } yield res.asInstanceOf[Tuple]
              }
          )
      )

    }
  }

  /**
    * Define the tuple-head primitive.
    * This returns the first valueent of a Tuple
    * e.g. (tuple-head [1 2 3 4 5 6]) ==> 1
    */
  object tplHead extends Prim {
    override val name: String = "tuple-head"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Tuple] // Only arg must be a Tuple
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Ob] = {
      val value = ctxt.argvec.value

      checkTuple(0, value).map { t => // Ensure arg0 is a Tuple
        t.nthLift(0) match {
          case Some(v: Ob) => v
          case None        => Nil
        }
      }
    }
  }

  /**
    * Define the tuple-last primitive.
    * This returns the last valueent of a Tuple
    * e.g. (tuple-last [1 2 3 4 5 6]) ==> 6
    */
  object tplLast extends Prim {
    override val name: String = "tuple-last"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Tuple] // Only arg must be a Tuple
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Ob] = {
      val value = ctxt.argvec.value

      checkTuple(0, value).map { t => // Ensure arg0 is a Tuple
        t.nthLift(t.value.length - 1) match {
          case Some(v: Ob) => v
          case None        => Nil
        }
      }
    }
  }

  /**
    * Define the tuple-tail primitive.
    * This returns the Tuple consisting of the 2nd through the nth
    * valueents of the provided Tuple
    * e.g. (tuple-tail [1 2 3 4 5 6]) ==> [2 3 4 5 6]
    */
  object tplTail extends Prim {
    override val name: String = "tuple-tail"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Tuple] // Only arg must be a Tuple
    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value = ctxt.argvec.value
      val t     = value(0).asInstanceOf[Tuple]
      if (t.value.length > 0)
        Right(t.makeTail(1))
      else
        Right(Nil)
    }
  }

  /**
    * Define the tuple-new primitive.
    * This returns a new Tuple containing the specified n Obs
    * e.g. (new Tuple 1 2 3 4 5 6) ==> [1 2 3 4 5 6]
    *
    * WARNING: this primitive (like tplNewN, tplexprNew and tplexprNewN)
    * takes an extra argument in the leading slot, allowing these
    * primitives to be bound directly to operations.  That is, the
    * operation new is bound to tplNew in a prototypical tuple Tuple,
    * then the expression
    *
    * 	(new Tuple a b c)
    *
    * will be equivalent to
    *
    * 	(tplNew Tuple a b c)
    *
    * Since we want to produce [a b c], we need to ignore the first arg.
    */
  object tplNew extends Prim {
    override val name: String = "tuple-new"
    override val minArgs: Int = 1
    override val maxArgs: Int = MaxArgs

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value = ctxt.argvec.value
      val nargs = ctxt.nargs

      Right(Tuple(value).makeSlice(1, nargs - 1))
    }
  }

  /**
    * Define the tuple-new-n primitive.
    * This returns a new Tuple containing the specified n duplicated Obs
    *
    * See warning in tplNew about the rationale for ignoring ARG(0).
    * 	(tplNewN dummy n init)
    *
    * e.g.(tuple-new-n [] 3 'a) ===>	['a 'a 'a]
    */
  object tplNewN extends Prim {
    override val name: String = "tuple-new-n"
    override val minArgs: Int = 3
    override val maxArgs: Int = 3

    override def fnSimple(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val value = ctxt.argvec.value

      checkFixnum(1, value).flatMap { n => // Ensure arg1 is a Fixnum
        if (n.value <= 0)
          Right(Nil)
        else
          Right(Tuple(n.value, value(2)))
      }
    }
  }

  object tplMemQ extends Prim {
    override val name: String = "tuple-mem?"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val value = ctxt.argvec.value

      checkTuple(0, value).map { t =>
        RblBool(t.value.contains(value(1)))
      }
    }
  }

  object tplMatchesP extends Prim {
    override val name: String = "tuple-matches?"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Tuple] // Args must be Tuples
    override def fnSimple(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val value = ctxt.argvec.value
      val pat   = value(0).asInstanceOf[Tuple]
      val tup   = value(1).asInstanceOf[Tuple]
      Right(RblBool(pat.matches(tup)))
    }
  }

  /** Helper functions begin here */
  /**
    * Check the specified parameter for type Tuple. Return a PrimError if it is
    * not else return the Tuple.
    */
  private def checkTuple(n: Int, value: Seq[Ob]): Either[PrimError, Tuple] =
    if (!value(n).isInstanceOf[Tuple]) {
      Left(TypeMismatch(n, Tuple.getClass.getName))
    } else {
      Right(value(n).asInstanceOf[Tuple])
    }

  /**
    * Check the specified parameter for type Fixnum. Return a PrimError if it is
    * not else return the Fixnum.
    */
  private def checkFixnum(n: Int, value: Seq[Ob]): Either[PrimError, Fixnum] =
    if (!value(n).isInstanceOf[Fixnum]) {
      Left(TypeMismatch(n, Fixnum.getClass.getName))
    } else {
      Right(value(n).asInstanceOf[Fixnum])
    }

}
