package coop.rchain.rosette

import org.scalatest.{FlatSpec, Matchers}

class QueueSpec extends FlatSpec with Matchers {

  "queue enQueue" should "correctly add ob" in {
    val q = Queue.create()
    q.enQueue(Fixnum(0))
    q.nth(0).contains(Fixnum(0)) should be(true)
  }

  "queue deQueue" should "correctly deQueue ob" in {
    val q = Queue.create()
    q.enQueue(Fixnum(0))
    q.nth(0).contains(Fixnum(0)) should be(true)
    q.deQueue()
    q.depth() should be(0)
    q.empty() should be(true)
  }

  "queue setNth" should "correctly set ob" in {
    val q = Queue.create()
    q.enQueue(Fixnum(0))
    q.enQueue(Fixnum(1))
    q.enQueue(Fixnum(2))
    q.enQueue(Fixnum(3))
    q.setNth(2, Fixnum(6))
    q.nth(2).contains(Fixnum(6)) should be(true)
  }

  "queue patternDequeue" should "correctly pattern dequeue ob" in {
    val q = Queue.create()
    q.enQueue(Tuple(1,Fixnum(0)))
    q.enQueue(Tuple(1,Fixnum(1)))
    q.enQueue(Tuple(1,Fixnum(2)))
    q.enQueue(Tuple(1,Fixnum(3)))
    q.patternDequeue(Tuple(1,Fixnum(2)))
    q.nth(2).contains(Tuple(1,Fixnum(3))) should be(true)
  }

  "queue patternRead" should "correctly pattern read ob" in {
    val q = Queue.create()
    q.enQueue(Tuple(1,Fixnum(0)))
    q.enQueue(Tuple(1,Fixnum(1)))
    q.enQueue(Tuple(1,Fixnum(2)))
    q.enQueue(Tuple(1,Fixnum(3)))
    q.patternRead(Tuple(1,Fixnum(3))).contains(Tuple(1,Fixnum(3))) should be(true)
  }

  "queue dequeueNth" should "correctly dequeue nth ob" in {
    val q = Queue.create()
    q.enQueue(Fixnum(0))
    q.enQueue(Fixnum(1))
    q.enQueue(Fixnum(2))
    q.enQueue(Fixnum(3))
    // 0 1 2 3
    q.dequeueNth(2).contains(Fixnum(2)) should be(true)
    // 0 1 3
    q.nth(2).contains(Fixnum(3)) should be(true)
  }

  "queue reset" should "correctly reset" in {
    val q = Queue.create()
    q.enQueue(Fixnum(0))
    q.enQueue(Fixnum(1))
    q.enQueue(Fixnum(2))
    q.enQueue(Fixnum(3))
    q.reset()
    q.depth() should be(0)
    q.empty() should be(true)
  }

}
