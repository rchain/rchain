package coop.rchain.pcasper.finalization.eager

import coop.rchain.pcasper.finalization.Fringe
import coop.rchain.pcasper.finalization.eager.EagerFinal.LevelReconciliation

import scala.annotation.tailrec

// View on DAG level
final case class LevelView[M, S](height: Long, partitions: Vector[Fringe[M, S]]) {
  def fringe: Fringe[M, S] = partitions.reduceOption(_ ++ _).getOrElse(Map())
  def stake(bondsMap: Map[S, Long]): Long =
    fringe.keysIterator.map { s =>
      val bond = bondsMap.get(s)
      assert(bond.isDefined, "Level view contains message from not bonded sender.")
      bond.get
    }.sum
}

object LevelView {

  def reconcile[M, S](
      views: Vector[LevelView[M, S]]
  )(bondsMap: Map[S, Long]): LevelReconciliation[M, S] = {
    require(
      views.map(_.height).distinct.size == 1,
      "Unexpected error: reconciliation across different levels is detected."
    )

    final case class PastureState(
        amoeba: Vector[(Fringe[M, S], Int)],          // indexed legs of amoeba
        pasture: Vector[Vector[(Fringe[M, S], Int)]], // indexed pieces of food in the pasture
        legsAdjusted: Vector[Fringe[M, S]]            // legs of amoeba that absorbed something, so changed size
    )

    // Amoeba algo - suck in smaller intersecting partitions
    @tailrec
    def absorb(state: PastureState): PastureState = {
      // check each corner of the pasture
      val newState = state.pasture.foldLeft(state) {
        case (s, food) =>
          // check each leg of amoeba over food in the corner
          val newS = state.amoeba.foldLeft((s, food)) {
            case (in @ (s_, f), (leg, lIdx)) =>
              val allEaten = f.isEmpty
              if (allEaten) in
              else {
                val yum = f.filter { case (v, _) => (v.keySet intersect leg.keySet).nonEmpty }
                val (newAmoeba, newFood) = yum.foldLeft(s_.amoeba, f) {
                  case ((amoeba, f_), (eaten, idx)) =>
                    val newLeg = (s_.amoeba(idx)._1 ++ eaten, s_.amoeba(idx)._2)
                    val newAmoeba = (amoeba.take(idx - 1) :+ newLeg) ++
                      amoeba.takeRight(amoeba.size - idx + 1)
                    val foodLeft = f_.take(idx) ++ f_.takeRight(food.size - idx + 1)
                    (newAmoeba, foodLeft)
                }
                val newState =
                  PastureState(newAmoeba, Vector(newFood), state.legsAdjusted ++ yum.map(_._1))
                (newState, newFood)
              }
          }
          newS._1
      }
      val done = newState.amoeba.map(_._1) == state.amoeba.map(_._1) ||
        !state.pasture.exists(_.nonEmpty)
      if (done) state       // nothing more to eat, return
      else absorb(newState) // something has been eaten, amoeba grown, try to absorb more.
    }

    val sorted      = views.sortBy(_.stake(bondsMap)).reverse
    val initAmoeba  = sorted.map(_.partitions).head.zipWithIndex
    val initPasture = sorted.map(_.partitions).tail.map(_.zipWithIndex)
    val initState   = PastureState(initAmoeba, initPasture, Vector.empty[Fringe[M, S]])

    val r = absorb(initState)
    LevelReconciliation(
      LevelView(views.map(_.height).head, r.amoeba.map(_._1) ++ r.pasture.flatMap(_.map(_._1))),
      r.legsAdjusted
    )
  }
}
