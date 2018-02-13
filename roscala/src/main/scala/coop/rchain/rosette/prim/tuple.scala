package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Fixnum, Ob, Tuple}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

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

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem
      checkTuple(1, elem).map(Tuple.cons(elem.head, _)) // arg1 must be a Tuple
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

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem  = ctxt.argvec.elem
      val nargs = ctxt.nargs
      val last  = nargs - 1
      val slice = elem.slice(0, last)

      checkTuple(last, elem)
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

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem

      checkTuple(0, elem).map(Tuple.rcons(_, elem(1))) // arg0 must be a Tuple
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
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem
      val n    = ctxt.nargs
      val init = Tuple(Seq.empty)

      Right(
        elem.foldLeft(init)(
          (acc: Tuple, el: Ob) => Tuple(acc, el.asInstanceOf[Tuple])
        )
      )
    }
  }

  /**
    * Define the "tuple-safe-nth" primitive.
    * This safely returns the nth element of the specified Tuple
    * e.g. (tuple-safe-nth [1 2 3 4 5] 3) ==> 4
    */
  object tplSafeNth extends Prim {
    override val name: String = "tuple-safe-nth"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val elem = ctxt.argvec.elem

      checkTuple(0, elem).flatMap( // Ensure arg0 is a Tuple
        t =>
          checkFixnum(1, elem).map( // Ensure arg1 is a Fixnum
            n =>
              if (n.value < 0)
                Fixnum(Int.MinValue)
              else if (n.value < t.elem.size) {
                t.nth(n.value) match {
                  case Some(v: Fixnum) => v
                  case None            => Fixnum(Int.MaxValue)
                }
              } else
                Fixnum(Int.MaxValue)))
    }
  }

  /**
    * Define the "tuple-xchg" primitive.
    * This exchanges the nth and mth elements of the specified Tuple
    * e.g. (tuple-xchg [1 2 3 4 5] 1 3) ==> [1 4 3 2 5]
    */
  object tplXchg extends Prim {
    override val name: String = "tuple-xchg"
    override val minArgs: Int = 3
    override val maxArgs: Int = 3

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Tuple] = {
      val elem = ctxt.argvec.elem

      // Check and get arguments: Tuple, Fixnum, Fixnum
      checkTuple(0, elem).flatMap(
        t => // Ensure arg0 is a Tuple
          checkFixnum(1, elem).flatMap(n => // Ensure arg1 is a Fixnum
            checkFixnum(2, elem).flatMap { m => // Ensure arg2 is a Fixnum

              t.nth(n.value) match { // Extract nth
                case Some(nv: Fixnum) =>
                  t.nth(m.value) match { // Extract mth
                    case Some(mv: Fixnum) =>
                      t.setNth(n.value, mv) match { //  set mth
                        case Some(t1: Tuple) =>
                          t1.setNth(m.value, nv) match { // Set nth
                            case Some(ret: Tuple) => Right(ret) // Success
                            case None =>
                              Left(IndexOutOfBounds(m.value, t.elem.size)) // Fail set nth
                          }
                        case None => Left(IndexOutOfBounds(n.value, t.elem.size)) // Fail set mth
                      }
                    case None => Left(IndexOutOfBounds(m.value, t.elem.size)) // Fail extract mth
                  }
                case None => Left(IndexOutOfBounds(n.value, t.elem.size)) // Fail extract nth
              }

          }))
    }
  }

  /** Helper functions begin here */
  /**
    * Check the specified parameter for type Tuple. Return a PrimError if it is
    * not else return the Tuple.
    */
  private def checkTuple(n: Int, elem: Seq[Ob]): Either[PrimError, Tuple] =
    if (!elem(n).isInstanceOf[Tuple]) {
      Left(TypeMismatch(n, Tuple.getClass.getName))
    } else {
      Right(elem(n).asInstanceOf[Tuple])
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

}
