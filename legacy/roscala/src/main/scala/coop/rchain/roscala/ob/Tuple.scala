package coop.rchain.roscala.ob

class Tuple(val value: Array[Ob]) extends Ob {

  def apply(n: Int): Ob = value(n)

  override def accepts(msg: Ctxt): Boolean =
    value.exists(_.matches(msg))

  def becomeExtension(newMeta: Meta, newParent: Ob): Extension = {
    val extension = new Extension()
    extension.meta = newMeta
    extension.parent = newParent
    extension.slot ++= value

    extension
  }

  def elem(n: Int): Ob = value(n)

  def update(arg: Int, ob: Ob): Unit = value.update(arg, ob)

  def makeSlice(offset: Int, n: Int): Tuple = Tuple(n, this, offset, n)

  def makeTail(entriesToSkip: Int): Tuple = {
    val size = numberOfElements() - entriesToSkip
    Tuple(size, this, entriesToSkip, size)
  }

  override def indexedSize(): Fixnum = Fixnum(this.numberOfElements())

  override def matches(msg: Ctxt): Boolean = {
    val n = numberOfElements()

    if (n > 0 && n <= msg.nargs) {
      if (this.value.head != msg.trgt) {
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

        this.value
          .drop(1)
          .zip(msg.argvec.value.drop(1))
          .forall {
            case (e, arg) => e == arg
          }
      }
    } else false
  }

  def matches(msg: Tuple): Boolean = {
    val n = this.value.length

    if (n > 0 && n <= msg.value.length) {

      this.value
        .zip(msg.value)
        .forall {
          case (e, msgElem) =>
            if (e != msgElem && e != Niv) {
              if (e.isInstanceOf[Tuple] && msgElem.isInstanceOf[Tuple]) {
                if (e.asInstanceOf[Tuple].matches(msgElem.asInstanceOf[Tuple])) {
                  true
                } else false
              } else false
            } else true
        }

    } else false
  }

  def nth(n: Int): Ob = this.value(n)

  def nthLift(n: Int): Option[Ob] = this.value.lift(n)

  def setNth(n: Int, ob: Ob): Tuple = {
    this.value.update(n, ob)
    this
  }

  def setNthLift(n: Int, ob: Ob): Option[Ob] =
    try {
      Some(setNth(n, ob))
    } catch {
      case _: IndexOutOfBoundsException => None
    }

  def subObject(i: Int, n: Int): Tuple = makeSlice(i, n)

  /* Assumes that last element in `Tuple` is a `Tuple` */
  def unwind(): Tuple = {
    val prefix = this.value.take(this.value.length - 1)
    val rest   = this.value.last.asInstanceOf[Tuple]
    new Tuple(prefix ++ rest.value)
  }

  def numberOfElements(): Int = value.length
}

object Tuple {
  val tupleMeta = Meta(extensible = false)
  val tupleSbo  = new Actor()

  def apply(size: Int, master: Tuple, offset: Int, n: Int, init: Option[Ob] = None): Tuple = {
    val slice = master.value.slice(offset, n + offset)

    val filling: Array[Ob] = if (size > n && init.isDefined) {
      Array.fill(size - n)(init.get)
    } else {
      Array.empty
    }

    Tuple(slice ++ filling)
  }

  def apply(n: Int, b: Ob): Tuple =
    if (n == 0)
      Nil
    else
      Tuple(Array.fill(n)(b))

  def apply(value: Array[Ob]): Tuple = wire(new Tuple(value))

  def apply(obs: Ob*): Tuple = wire(new Tuple(obs.toArray[Ob]))

  def apply(t1: Tuple, t2: Tuple): Tuple = Tuple(t1.value ++ t2.value)

  def rcons(t: Tuple, ob: Ob): Tuple = Tuple(t.value :+ ob)

  def cons(ob: Ob, t: Tuple): Tuple = Tuple(ob +: t.value)

  def concat(t1: Tuple, t2: Tuple): Tuple = apply(t1, t2)

  def wire(tuple: Tuple): Tuple = {
    tuple.parent = tupleSbo
    tuple.meta = tupleMeta
    tuple.meta.refCount.incrementAndGet()
    tuple
  }
}

object Nil extends Tuple(Array.empty[Ob]) {
  override def accepts(msg: Ctxt): Boolean = true

  override def makeSlice(offset: Int, n: Int): Tuple = Nil

  override def matches(msg: Tuple): Boolean = true
}
