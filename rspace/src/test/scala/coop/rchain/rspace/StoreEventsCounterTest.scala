package coop.rchain.rspace

import org.scalatest.{FlatSpec, Matchers}

class TestStoreEventsCount extends StoreEventsCounter {
  var curTime: Long                     = 1111111L
  protected override def nanoTime: Long = curTime

  def sleep(milliseconds: Int) = curTime += milliseconds * 1000000
}

class StoreEventsCounterTest extends FlatSpec with Matchers {
  "event counter" should
    "properly calculate 3 events per second" in {

    val cnt = new TestStoreEventsCount()

    cnt.registerProduce {}
    cnt.createCounters(0, 0).producesCount.count shouldBe 1
    cnt.createCounters(0, 0).producesCount.currentRate shouldBe 1
    cnt.createCounters(0, 0).producesCount.peakRate shouldBe 1

    cnt.sleep(100)
    cnt.registerProduce {}
    cnt.createCounters(0, 0).producesCount.count shouldBe 2
    cnt.createCounters(0, 0).producesCount.currentRate shouldBe 2
    cnt.createCounters(0, 0).producesCount.peakRate shouldBe 2

    cnt.sleep(100)
    cnt.registerProduce {}
    cnt.createCounters(0, 0).producesCount.count shouldBe 3
    cnt.createCounters(0, 0).producesCount.currentRate shouldBe 3
    cnt.createCounters(0, 0).producesCount.peakRate shouldBe 3

    cnt.sleep(850)
    cnt.createCounters(0, 0).producesCount.count shouldBe 3
    cnt.createCounters(0, 0).producesCount.currentRate shouldBe 2
    cnt.createCounters(0, 0).producesCount.peakRate shouldBe 3

    cnt.sleep(200)
    cnt.createCounters(0, 0).producesCount.count shouldBe 3
    cnt.createCounters(0, 0).producesCount.currentRate shouldBe 0
    cnt.createCounters(0, 0).producesCount.peakRate shouldBe 3
  }
}
