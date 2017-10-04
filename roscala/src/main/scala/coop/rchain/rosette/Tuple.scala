package coop.rchain.rosette

/** Tuple
  *
  *  Tuples are used to represent messages that convey information
  *  between two actors in a communication, to introduce sub-structure
  *  in messages, and as a primitive data structure in the definition
  *  of other actors
  */
case class Tuple(elem: Seq[Ob],
                 override val parent: Ob,
                 override val meta: Ob,
                 override val slot: Seq[Ob])
    extends Ob {

  def makeSlice(offset: Int, n: Int): Tuple =
    this match {
      case Tuple.NIL => Tuple.NIL
      case _ => Tuple(n, this, offset, n)
    }

  def makeTail(entriesToSkip: Int): Tuple = {
    val size = this.elem.size - entriesToSkip
    Tuple(size, this, entriesToSkip, size)
  }

  def nth(n: Int): Option[Ob] = this.elem.lift(n)

  def setNth(n: Int, ob: Ob): Option[Tuple] =
    try {
      Some(Tuple(this.elem.updated(n, ob), this.parent, this.meta, this.slot))
    } catch {
      case _: IndexOutOfBoundsException => None
    }

  def subObject(i: Int, n: Int): Tuple = makeSlice(i, n)

  def accepts(msg: Ctxt): Boolean =
    if (this == Tuple.NIL) {
      true
    } else {
      this.elem.exists(_.matches(msg))
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

}

object Tuple {

  object NIL extends Tuple(null, null, null, null)

  val PLACEHOLDER = new Tuple(Seq(), null, null, null)

  def apply(init: Ob) = new Tuple(Seq(init), null, null, null)

  def apply(t1: Tuple, t2: Tuple): Tuple =
    new Tuple(t1.elem ++ t2.elem, null, null, null)

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

    new Tuple(slice ++ filling, null, null, null)
  }

  def apply(a: Int, b: Option[Ob]): Tuple =
    new Tuple(null, null, null, null)

  def cons(ob: Ob, t: Tuple): Tuple =
    new Tuple(ob +: t.elem, null, null, null)

  def rcons(t: Tuple, ob: Ob): Tuple =
    new Tuple(t.elem :+ ob, null, null, null)

  def concat(t1: Tuple, t2: Tuple): Tuple = apply(t1, t2)
}
