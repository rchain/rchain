package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.queue._
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

  val queue      = Queue(Tuple(2, Some(Number(0))))
  val emptyQueue = Queue.create()

  "queue-depth" should "correctly get queue's depth" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(queue))
    queueDepth.fn(ctxt) should be(Right(Number(0)))
    queueDepth.fn(newCtxt) should be(Right(Number(2)))
  }

  "queue-empty?" should "correctly get queue's depth" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(queue))
    queueIsEmpty.fn(ctxt) should be(Right(RblBool(true)))
    queueIsEmpty.fn(newCtxt) should be(Right(RblBool(false)))
  }

  "queue-enqueue" should "correctly enqueue a ob" in {
    val ar       = Tuple.cons(queue, Tuple(1, Number(1)))
    val newCtxt  = ctxt.copy(nargs = 2, argvec = ar)
    val newQueue = Queue(Tuple(2, Some(Number(0))))
    val ret      = queueEnqueue.fn(newCtxt)

    ret should be(Right(newQueue.enqueue(Number(1))))
  }

  "queue-dequeue" should "correctly dequeue a ob" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Queue(Tuple(3, Some(Number(0))))))
    queueDeQueue.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-read" should "correctly read a ob" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(Queue(Tuple(3, Some(Number(0))))))
    queueRead.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-pat-dequeue" should "correctly pat dequeue a ob" in {
    // Tuple(Queue(Tuple), Tuple)
    val newQueue1 = Queue(Tuple(Tuple(1, Some(Number(0)))))
    val newCtxt1  = ctxt.copy(nargs = 2, argvec = Tuple.cons(newQueue1, Tuple(Tuple(Number(0)))))
    queuePDequeue.fn(newCtxt1) should be(Right(Tuple(List(Number(0)))))

    val newQueue2 = Queue(Tuple(1, Some(Number(0)))).enqueue(Tuple(1, Some(Number(0))))
    val newCtxt2  = ctxt.copy(nargs = 2, argvec = Tuple.cons(newQueue2, Tuple(Tuple(Number(0)))))
    queuePDequeue.fn(newCtxt2) should be(Right(Tuple(Number(0))))
  }

  "queue-pat-read" should "correctly pat read a ob" in {
    val newQueue = Queue(Tuple(Tuple(1, Some(Number(0)))))
    val newCtxt  = ctxt.copy(nargs = 2, argvec = Tuple.cons(newQueue, Tuple(Tuple(Number(0)))))
    queuePatRead.fn(newCtxt) should be(Right(Tuple(Number(0))))
  }

  "queue-read-nth" should "correctly read nth ob" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.cons(queue, Tuple(1, Number(1))))
    queueReadNth.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-dequeue-nth" should "correctly dequeue nth ob" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.cons(queue, Tuple(1, Number(0))))
    queueDequeueNth.fn(newCtxt) should be(Right(Number(0)))
  }

  "queue-reset" should "correctly reset" in {
    val newCtxt = ctxt.copy(nargs = 1, argvec = Tuple(queue))
    queueReset.fn(newCtxt) should be(Right(emptyQueue))
  }

}
