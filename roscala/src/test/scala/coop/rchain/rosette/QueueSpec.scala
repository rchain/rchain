package coop.rchain.rosette

import org.scalatest.{FlatSpec, Matchers}

class QueueSpec extends FlatSpec with Matchers {

  "queue enQueue" should "correctly add ob" in {
    val q = Queue.create()
    val q1 = q.enQueue(Fixnum(0))
    q1.nth(0).contains(Fixnum(0)) should be(true)
  }

  "queue deQueue" should "correctly deQueue ob" in {
    val q = Queue.create()
    val q1 = q.enQueue(Fixnum(0))
    q1.nth(0).contains(Fixnum(0)) should be(true)
    val q2 = q1.deQueue().get
    q2.depth() should be(0)
    q2.isEmpty() should be(true)
  }

  "queue setNth" should "correctly set ob" in {
    val q = Queue.create()
    val q1 = q.enQueue(Fixnum(0))
    val q2 = q1.enQueue(Fixnum(1))
    val q3 = q2.enQueue(Fixnum(2))
    val q4 = q3.enQueue(Fixnum(3))
    val q5 = q4.setNth(2, Fixnum(6)).get
    q5.nth(2).contains(Fixnum(6)) should be(true)
  }

  "queue patternDequeue" should "correctly pattern dequeue ob" in {
    val q = Queue.create()
    val q1 = q.enQueue(Tuple(1, Fixnum(0)))
    val q2 = q1.enQueue(Tuple(1, Fixnum(1)))
    val q3 = q2.enQueue(Tuple(1, Fixnum(2)))
    val q4 = q3.enQueue(Tuple(1, Fixnum(3)))
    val q5 = q4.patternDequeue(Tuple(1, Fixnum(2))).get
    q5.nth(2).contains(Tuple(1, Fixnum(3))) should be(true)
  }

  "queue patternRead" should "correctly pattern read ob" in {
    val q = Queue.create()
    val q1 = q.enQueue(Tuple(1, Fixnum(0)))
    val q2 = q1.enQueue(Tuple(1, Fixnum(1)))
    val q3 = q2.enQueue(Tuple(1, Fixnum(2)))
    val q4 = q3.enQueue(Tuple(1, Fixnum(3)))
    q4.patternRead(Tuple(1, Fixnum(3)))
      .contains(Tuple(1, Fixnum(3))) should be(true)
  }

  "queue dequeueNth" should "correctly dequeue nth ob" in {
    val q = Queue.create()
    val q1 = q.enQueue(Fixnum(0))
    val q2 = q1.enQueue(Fixnum(1))
    val q3 = q2.enQueue(Fixnum(2))
    val q4 = q3.enQueue(Fixnum(3))
    // 0 1 2 3
    val q5 = q4.dequeueNth(2)
    q5.get.elems.elem.contains(Fixnum(2)) should be(false)
    q5.get.elems.elem.contains(Fixnum(0)) should be(true)
  }

  "queue reset" should "correctly reset" in {
    val q = Queue.create()
    val q1 = q.enQueue(Fixnum(0))
    val q2 = q1.reset()
    q2.depth() should be(0)
    q2.isEmpty() should be(true)
  }

}
