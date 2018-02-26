package coop.rchain.rosette.prim

import coop.rchain.rosette.{Ctxt, Fixnum, Ob, RblBool, RblString}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

object rblstring {

  /** Case sensitive comparisons */
  object stringEq extends Prim {
    override val name: String = "string="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value == s2.value))
    }
  }

  object stringNEq extends Prim {
    override val name: String = "string!="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value != s2.value))
    }
  }

  object stringLess extends Prim {
    override val name: String = "string<"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value < s2.value))
    }
  }

  object stringLEq extends Prim {
    override val name: String = "string<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value <= s2.value))
    }
  }

  object stringGtr extends Prim {
    override val name: String = "string>"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value > s2.value))
    }
  }

  object stringGEq extends Prim {
    override val name: String = "string<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value >= s2.value))
    }
  }

  /** Case Insensitive conparisons */
  object stringCiEq extends Prim {
    override val name: String = "string-ci="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value.toLowerCase == s2.value.toLowerCase))
    }
  }

  object stringCiNEq extends Prim {
    override val name: String = "string-ci!="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value.toLowerCase != s2.value.toLowerCase))
    }
  }

  object stringCiLess extends Prim {
    override val name: String = "string-ci<"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value.toLowerCase < s2.value.toLowerCase))
    }
  }

  object stringCiLEq extends Prim {
    override val name: String = "string-ci<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value.toLowerCase <= s2.value.toLowerCase))
    }
  }

  object stringCiGtr extends Prim {
    override val name: String = "string-ci>"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value.toLowerCase > s2.value.toLowerCase))
    }
  }

  object stringCiGEq extends Prim {
    override val name: String = "string-ci<="
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[RblString] // Args must be strings
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val elem = ctxt.argvec.elem
      val s1   = elem(0).asInstanceOf[RblString]
      val s2   = elem(1).asInstanceOf[RblString]
      Right(RblBool(s1.value.toLowerCase >= s2.value.toLowerCase))
    }
  }

}
