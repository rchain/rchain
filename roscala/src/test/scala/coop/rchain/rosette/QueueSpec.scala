package coop.rchain.rosette

import org.scalatest.{FlatSpec, Matchers}

class QueueSpec extends FlatSpec with Matchers {

  val q = Queue(Tuple.Placeholder)

  "queue enQueue" should "correctly add ob" in {
    val q1 = q.enqueue(Fixnum(0))
    q1.nth(0).contains(Fixnum(0)) should be(true)
  }

  "queue deQueue" should "correctly deQueue ob" in {
    val q1 = q.enqueue(Fixnum(0))
    q1.nth(0).contains(Fixnum(0)) should be(true)
    val q2 = q1.dequeue()._1
    q2.depth() should be(0)
    q2.isEmpty() should be(true)
  }

  "queue setNth" should "correctly set ob" in {
    val q1 = q.enqueue(Fixnum(0))
    val q2 = q1.enqueue(Fixnum(1))
    val q3 = q2.enqueue(Fixnum(2))
    val q4 = q3.enqueue(Fixnum(3))
    val q5 = q4.setNth(2, Fixnum(6)).get
    q5.nth(2).contains(Fixnum(6)) should be(true)
  }

  "queue patternDequeue" should "correctly pattern dequeue ob" in {
    val q1 = q.enqueue(Tuple(1, Fixnum(0)))
    val q2 = q1.enqueue(Tuple(1, Fixnum(1)))
    val q3 = q2.enqueue(Tuple(1, Fixnum(2)))
    val q4 = q3.enqueue(Tuple(1, Fixnum(3)))
    val q5 = q4.patternDequeue(Tuple(1, Fixnum(2)))._1
    q5.nth(2).contains(Tuple(1, Fixnum(3))) should be(true)
  }

  "queue patternRead" should "correctly pattern read ob" in {
    val q1 = q.enqueue(Tuple(1, Fixnum(0)))
    val q2 = q1.enqueue(Tuple(1, Fixnum(1)))
    val q3 = q2.enqueue(Tuple(1, Fixnum(2)))
    val q4 = q3.enqueue(Tuple(1, Fixnum(3)))
    q4.patternRead(Tuple(1, Fixnum(3))) == Tuple(1, Fixnum(3)) should be(true)
  }

  "queue dequeueNth" should "correctly dequeue nth ob" in {
    val q1 = q.enqueue(Fixnum(0))
    val q2 = q1.enqueue(Fixnum(1))
    val q3 = q2.enqueue(Fixnum(2))
    val q4 = q3.enqueue(Fixnum(3))
    // 0 1 2 3
    val q5 = q4.dequeueNth(2)._1
    q5.elems.elem.contains(Fixnum(2)) should be(false)
    q5.elems.elem.contains(Fixnum(0)) should be(true)
  }

  "queue reset" should "correctly reset" in {
    val q1 = q.enqueue(Fixnum(0))
    val q2 = q1.reset()
    q2.depth() should be(4)
    q2.isEmpty() should be(false)
  }

}
