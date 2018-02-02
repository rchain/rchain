package coop.rchain.rosette.prim

import coop.rchain.rosette.Ob.ABSENT
import coop.rchain.rosette.{Ctxt, Fixnum, Ob, Queue, RblBool, Tuple}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

object PrimQueue {

  object QueueNew extends Prim {
    override val name: String = "queue-new"
    override val minArgs: Int = 0
    override val maxArgs: Int = 0

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Queue] =
      Right(Queue.create())
  }

  object QueueDepth extends Prim {
    override val name: String = "queue-depth"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      Right(q.nElem.asInstanceOf[Fixnum])
    }
  }

  object QueueEmpty extends Prim {
    override val name: String = "queue-empty?"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      Right(RblBool(q.empty()))
    }
  }


  object QueueEnQueue extends Prim {
    override val name: String = "queue-enqueue"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Ob]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Queue] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      val ob = ctxt.argvec.elem(1)
      q.enQueue(ob)
      Right(q)
    }
  }

  object QueueDeQueue extends Prim {
    override val name: String = "queue-dequeue"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Ob]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      if(q.empty()) {
        Left(QueueEmptyError("queue is empty"))
      }
      Right(q.deQueue())
    }
  }

  object QueueRead extends Prim {
    override val name: String = "queue-read"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      if(q.empty()) {
        Left(QueueEmptyError("queue is empty"))
      }
      Right(q.head)
    }
  }

  object QueuePatDeQueue extends Prim {
    override val name: String = "queue-pat-dequeue"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Ob]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      val pat = ctxt.argvec.elem(1).asInstanceOf[Tuple]
      if(q.empty()) {
        Left(QueueEmptyError("queue is empty"))
      } else if(pat == Tuple.NIL) {
        Right(q.deQueue())
      } else {
        Right(q.patternDequeue(pat).getOrElse(ABSENT))
      }
    }
  }

  object QueuePatRead extends Prim {
    override val name: String = "queue-pat-read"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Ob]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      val pat = ctxt.argvec.elem(1).asInstanceOf[Tuple]
      if(q.empty()) {
        Left(QueueEmptyError("queue is empty"))
      } else if(pat == Tuple.NIL) {
        val e = q.elems.elem.head
        Right(e)
      } else {
        Right(q.patternRead(pat).getOrElse(ABSENT))
      }
    }
  }

  object QueueReadNth extends Prim {
    override val name: String = "queue-read-nth"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Ob]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum].value
      if(q.empty()) {
        Left(QueueEmptyError("queue is empty"))
      }
      if(n > q.depth() || n < 0) {
        Left(ArgumentMismatch)
      }
      Right(q.elems.elem(n))
    }
  }

  object QueueDequeueNth extends Prim {
    override val name: String = "queue-dequeue-nth"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkTypeMismatch[Ob]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      val n = ctxt.argvec.elem(1).asInstanceOf[Fixnum].value
      if(q.empty()) {
        Left(QueueEmptyError("queue is empty"))
      }
      Right(q.dequeueNth(n).getOrElse(ABSENT))
    }
  }

  object QueueReset extends Prim {
    override val name: String = "queue-reset"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Queue] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      q.reset()
      Right(q)
    }
  }

}
