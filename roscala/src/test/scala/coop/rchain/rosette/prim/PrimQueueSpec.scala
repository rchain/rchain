package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.PrimQueue._
import coop.rchain.rosette.{Ctxt, PC, Queue, RblBool, Tuple, Fixnum => Number}
import org.scalatest.{FlatSpec, Matchers}

class PrimQueueSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0,
    pc = PC.PLACEHOLDER,
    rslt = null,
    trgt = null,
    argvec = Tuple(Queue(Tuple.Placeholder)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null,
  )

  val queue = Queue(Tuple(2, Some(Number(0))))
  val emptyQueue = Queue(Tuple.Placeholder)

  "queue-depth" should "correctly get queue's depth" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(queue))
    QueueDepth.fn(ctxt) should be(Right(Number(0)))
    QueueDepth.fn(newCtxt) should be(Right(Number(2)))
  }

  "queue-empty?" should "correctly get queue's depth" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(queue))
    QueueEmpty.fn(ctxt) should be(Right(RblBool(true)))
    QueueEmpty.fn(newCtxt) should be(Right(RblBool(false)))
  }

  "queue-enqueue" should "correctly enqueue a ob" in {
    val ar = Tuple.cons(queue, Tuple(1, Number(1)))
    val newCtxt = ctxt.copy(nargs = 2, argvec = ar)
    val newQueue = Queue(Tuple(2, Some(Number(0))))
    newQueue.enQueue(Number(1))
    val ret = QueueEnQueue.fn(newCtxt)

     ret should be(Right(newQueue))
  }

  "queue-dequeue" should "correctly dequeue a ob" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Queue( Tuple(3, Some(Number(0))))))
    QueueDeQueue.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-read" should "correctly read a ob" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Queue( Tuple(3, Some(Number(0))))))
    QueueRead.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-pat-dequeue" should "correctly pat dequeue a ob" in {
    // Tuple(Queue(Tuple), Tuple)
    val newQueue = Queue(Tuple(Tuple(1, Some(Number(0)))))
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.cons(newQueue, Tuple(Tuple(Number(0)))))
    QueuePatDeQueue.fn(newCtxt) should be(Right(Tuple(Number(0))))
  }

  "queue-pat-read" should "correctly pat read a ob" in {
    val newQueue = Queue(Tuple(Tuple(1, Some(Number(0)))))
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.cons(newQueue, Tuple(Tuple(Number(0)))))
    QueuePatRead.fn(newCtxt) should be(Right(Tuple(Number(0))))
  }

  "queue-read-nth" should "correctly read nth ob" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.cons(queue, Tuple(1, Number(1))))
    QueueReadNth.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-dequeue-nth" should "correctly dequeue nth ob" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.cons(queue, Tuple(1, Number(0))))
    QueueDequeueNth.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-reset" should "correctly reset" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(queue))
    QueueReset.fn(newCtxt) should be(Right(emptyQueue))
  }

}
