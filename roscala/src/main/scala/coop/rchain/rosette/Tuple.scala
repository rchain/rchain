package coop.rchain.rosette

/** Tuple
  *
  *  Tuples are used to represent messages that convey information
  *  between two actors in a communication, to introduce sub-structure
  *  in messages, and as a primitive data structure in the definition
  *  of other actors
  */
sealed trait TupleError
case object AbsentRest extends TupleError
case object InvalidRest extends TupleError

case class Tuple(elem: Seq[Ob]) extends Ob {

  def accepts(msg: Ctxt): Boolean =
    if (this == Tuple.NIL) {
      true
    } else {
      this.elem.exists(_.matches(msg))
    }

  /** Transforms a Tuple into a StdExtension */
  def becomeExtension(newMeta: Ob, newParent: Ob): StdExtension =
    StdExtension(newMeta, newParent, elem)

  def flattenRest(): Either[TupleError, Tuple] =
    this.elem.lastOption match {
      case Some(t: Tuple) if t != Tuple.NIL =>
        Right(Tuple(this.makeSlice(0, this.elem.size - 1), t))
      case Some(ob) => Left(InvalidRest)
      case None => Left(AbsentRest)
    }

  def makeSlice(offset: Int, n: Int): Tuple =
    this match {
      case Tuple.NIL => Tuple.NIL
      case _ => Tuple(n, this, offset, n)
    }

  def makeTail(entriesToSkip: Int): Tuple = {
    val size = this.elem.size - entriesToSkip
    Tuple(size, this, entriesToSkip, size)
  }

  override def matches(msg: Ctxt): Boolean = {
    val n = this.elem.size

    if (n > 0 && n <= msg.argvec.elem.size) {
      if (this.elem.head != msg.trgt) {
        false
      } else {

        /*
         * Skip msg.head, since it is actually the receiver of the
         * message, i.e.,
         *
         * 	(oprn rcvr arg1 arg2 ...)
         *
         * results in an argvec of
         *
         * 	[rcvr arg1 arg2 ...]
         */

        this.elem
          .drop(1)
          .zip(msg.argvec.elem)
          .forall {
            case (e, arg) => e == arg
          }
      }
    } else {
      false
    }
  }

  def matches(msg: Tuple): Boolean =
    if (this == Tuple.NIL) {
      true
    } else {
      val n = this.elem.size

      if (n > 0 && n <= msg.elem.size) {

        this.elem
          .zip(msg.elem)
          .forall {
            case (e, msgElem) =>
              if (e != msgElem && e != Ob.NIV) {
                if (e.meta.isInstanceOf[Tuple]
                    && msgElem.meta.isInstanceOf[Tuple]
                    && e.isInstanceOf[Tuple]
                    && msgElem.isInstanceOf[Tuple]) {
                  if (e.asInstanceOf[Tuple]
                        .matches(msgElem.asInstanceOf[Tuple])) {
                    true
                  } else false
                } else false
              } else true
          }

      } else false
    }

  override def nth(n: Int): Option[Ob] = this.elem.lift(n)

  override def setNth(n: Int, ob: Ob): Option[Tuple] =
    try {
      Some(Tuple(this.elem.updated(n, ob)))
    } catch {
      case _: IndexOutOfBoundsException => None
    }

  override def subObject(i: Int, n: Int): Tuple = makeSlice(i, n)

}

object Tuple {

  object NIL extends Tuple(Seq.empty)

  val Placeholder = Tuple(Seq.empty)

  def apply(init: Ob): Tuple =
    Tuple(Seq(init))

  def apply(t1: Tuple, t2: Tuple): Tuple =
    new Tuple(t1.elem ++ t2.elem)

  def apply(size: Int,
            master: Tuple,
            offset: Int,
            n: Int,
            init: Option[Ob] = None): Tuple = {
    val slice = master.elem.slice(offset, n + offset)

    val filling = if (size > n && init.isDefined) {
      Seq.fill(size - n)(init.get)
    } else {
      Seq.empty
    }

    new Tuple(slice ++ filling)
  }

  def apply(a: Int, b: Ob): Tuple = new Tuple(Seq.fill(a)(b))

  def apply(a: Int, b: Option[Ob]): Tuple =
    Tuple(Seq.fill(a)(b getOrElse Ob.INVALID))

  def cons(ob: Ob, t: Tuple): Tuple =
    new Tuple(ob +: t.elem)

  def rcons(t: Tuple, ob: Ob): Tuple =
    new Tuple(t.elem :+ ob)

  def concat(t1: Tuple, t2: Tuple): Tuple = apply(t1, t2)
}
