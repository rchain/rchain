package coop.rchain.rosette.prim

import coop.rchain.rosette.Ob.ABSENT
import coop.rchain.rosette.{Absent, Ctxt, Fixnum, Ob, PrimErrorWrapper, Queue, RblBool, Tuple}
import coop.rchain.rosette.macros.{checkArgumentMismatch, checkTypeMismatch}
import coop.rchain.rosette.prim.Prim._

object queue {

  object queueNew extends Prim {
    override val name: String = "queue-new"
    override val minArgs: Int = 0
    override val maxArgs: Int = 0

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Queue] =
      Right(Queue.create())
  }

  object queueDepth extends Prim {
    override val name: String = "queue-depth"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Fixnum] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      Right(q.nElems.asInstanceOf[Fixnum])
    }
  }

  object queueIsEmpty extends Prim {
    override val name: String = "queue-empty?"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, RblBool] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      Right(RblBool(q.isEmpty()))
    }
  }

  object queueEnqueue extends Prim {
    override val name: String = "queue-enqueue"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Queue] =
      ctxt.argvec.elem.head match {
        case q: Queue =>
          val ob = ctxt.argvec.elem(1)
          Right(q.enqueue(ob))
        case _ =>
          Left(ArgumentMismatch("The first element should be a queue"))
      }
  }

  object queueDeQueue extends Prim {
    override val name: String = "queue-dequeue"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] =
      ctxt.argvec.elem.head match {
        case q: Queue =>
          if (q.isEmpty()) {
            Right(ABSENT)
          } else {
            Right(q.dequeue().get)
          }
        case _ =>
          Left(ArgumentMismatch("The first element should be a queue"))
      }
  }

  object queueRead extends Prim {
    override val name: String = "queue-read"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      if (q.isEmpty()) {
        Left(Absent)
      }
      Right(q.head)
    }
  }

  object queuePDequeue extends Prim {
    override val name: String = "queue-pat-dequeue"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] =
      (ctxt.argvec.elem.head, ctxt.argvec.elem(1)) match {
        case (q: Queue, pat: Tuple) =>
          if (q.isEmpty) {
            Right(ABSENT)
          } else if (pat == Tuple.NIL) {
            Right(q.dequeue().get)
          } else {
            q.patternDequeue(pat)
              .map(Right(_))
              .getOrElse(Left(PatternMatchError))
          }
        case _ =>
          Left(
            ArgumentMismatch(
              "The first element should be a queue and second element should be a tuple"))
      }
  }

  object queuePatRead extends Prim {
    override val name: String = "queue-pat-read"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] =
      (ctxt.argvec.elem.head, ctxt.argvec.elem(1)) match {
        case (q: Queue, pat: Tuple) =>
          if (q.isEmpty()) {
            Right(ABSENT)
          } else if (pat == Tuple.NIL) {
            val e = q.elems.elem.head
            Right(e)
          } else {
            Right(q.patternRead(pat).getOrElse(ABSENT))
          }
        case _ =>
          Left(
            ArgumentMismatch(
              "The first element should be a queue and second element should be a tuple"))
      }
  }

  object queueReadNth extends Prim {
    override val name: String = "queue-read-nth"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] =
      (ctxt.argvec.elem.head, ctxt.argvec.elem(1)) match {
        case (q: Queue, num: Fixnum) =>
          val n = num.value
          if (q.isEmpty()) {
            Left(Absent)
          }
          if (n > q.depth() || n < 0) {
            Left(ArgumentMismatch)
          }
          Right(q.elems.elem(n))
        case _ =>
          Left(
            ArgumentMismatch(
              "The first element should be a queue and second element should be a tuple"))
      }
  }

  object queueDequeueNth extends Prim {
    override val name: String = "queue-dequeue-nth"
    override val minArgs: Int = 2
    override val maxArgs: Int = 2

    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Ob] =
      (ctxt.argvec.elem.head, ctxt.argvec.elem(1)) match {
        case (q: Queue, n: Fixnum) =>
          if (q.isEmpty()) {
            Right(ABSENT)
          }
          Right(q.dequeueNth(n.value).getOrElse(ABSENT))
        case _ =>
          Left(
            ArgumentMismatch(
              "The first element should be a queue and second element should be a fixnum"))
      }
  }

  object queueReset extends Prim {
    override val name: String = "queue-reset"
    override val minArgs: Int = 1
    override val maxArgs: Int = 1

    @checkTypeMismatch[Queue]
    @checkArgumentMismatch
    override def fn(ctxt: Ctxt): Either[PrimError, Queue] = {
      val q = ctxt.argvec.elem.head.asInstanceOf[Queue]
      Right(q.reset())
    }
  }

}
