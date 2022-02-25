package coop.rchain.finalization.regression

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.sim.Simulation._
import coop.rchain.finalization.NetworkRunner
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

class R01_LateMsgMismatchSpec extends FlatSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global

  val sut = new NetworkRunner[Task]()

  "finalization" should "not fail with late message from sender" in pendingUntilFixed {
    val r        = sut.runRegression(regressionTest[Task]).runSyncUnsafe()
    val (end, _) = r.last
    val a = end.senders.toList.map(
      _.realFringes
        .map(_.toList.sortBy { case (k, _) => k.id }.map(_._2.id).toString())
    )
    println(a.mkString("\n"))
  }

  private def regressionTest[F[_]: Sync](
      runner: NetworkRunner[F],
      net: Network,
      name: String,
      enableOutput: Boolean
  ): F[(Network, Int)] = {
    import runner._

    for {
      // Just to begin for-comprehension from flatMap
      net1_     <- runSections(net, List((1, .0f)), s"start-$name", enableOutput)
      (net1, _) = net1_

      (n1022813136, n1083092637) = net.split(0.5096376f)
      n_167755778_               <- runSections(n1022813136, List((1, 0.6574136f)), "n1022813136", enableOutput)
      (n_167755778, _)           = n_167755778_
      n_1407371484_ <- runSections(
                        n1083092637,
                        List((4, 0.78288853f)),
                        "n1083092637",
                        enableOutput
                      )
      (n_1407371484, _)          = n_1407371484_
      n1316169810                = n_167755778 >|< n_1407371484
      (n_2131838046, n171242518) = n1316169810.split(0.24446732f)
      n1709027370_ <- runSections(
                       n_2131838046,
                       List((1, 0.35403788f)),
                       "n_2131838046",
                       enableOutput
                     )
      (n1709027370, _)             = n1709027370_
      n_936033305_                 <- runSections(n171242518, List((8, 0.8514173f)), "n171242518", enableOutput)
      (n_936033305, _)             = n_936033305_
      n_801110001                  = n1709027370 >|< n_936033305
      (n_1753456045, n_1181790855) = n_801110001.split(0.68316025f)
      n_1885490992_ <- runSections(
                        n_1753456045,
                        List((12, 0.80235255f)),
                        "n_1753456045",
                        enableOutput
                      )
      (n_1885490992, _) = n_1885490992_
      n_1112360861_ <- runSections(
                        n_1181790855,
                        List((10, 0.23738176f)),
                        "n_1181790855",
                        enableOutput
                      )
      (n_1112360861, _)          = n_1112360861_
      (n1502918575, n_36996899)  = n_1112360861.split(0.46712136f)
      n2137437950_               <- runSections(n1502918575, List((7, 0.8242769f)), "n1502918575", enableOutput)
      (n2137437950, _)           = n2137437950_
      n1281978524_               <- runSections(n_36996899, List((8, 0.67792714f)), "n_36996899", enableOutput)
      (n1281978524, _)           = n1281978524_
      n200080004                 = n1281978524 >|< n2137437950
      n_911563424                = n_1885490992 >|< n200080004
      (n1009429810, n_984777102) = n_911563424.split(0.15929568f)
      n1507742654_ <- runSections(
                       n1009429810,
                       List((2, 0.85035944f)),
                       "n1009429810",
                       enableOutput
                     )
      (n1507742654, _) = n1507742654_
      n_1346367828_ <- runSections(
                        n_984777102,
                        List((3, 0.6530022f)),
                        "n_984777102",
                        enableOutput
                      )
      (n_1346367828, _)           = n_1346367828_
      (n_1870652816, n_884652529) = n1507742654.split(0.4179837f)
      n221340169_                 <- runSections(n_1870652816, List((6, 0.707146f)), "n_1870652816", enableOutput)
      (n221340169, _)             = n221340169_
      n1791511529_ <- runSections(
                       n_884652529,
                       List((4, 0.08678591f)),
                       "n_884652529",
                       enableOutput
                     )
      (n1791511529, _)          = n1791511529_
      n1316284692               = n1791511529 >|< n_1346367828
      (n_973792882, n123943753) = n1316284692.split(0.5909483f)
      n_950423999_ <- runSections(
                       n_973792882,
                       List((7, 0.12110078f)),
                       "n_973792882",
                       enableOutput
                     )
      (n_950423999, _)           = n_950423999_
      n830674740_                <- runSections(n123943753, List((4, 0.053336143f)), "n123943753", enableOutput)
      (n830674740, _)            = n830674740_
      n1773789275                = n830674740 >|< n_950423999
      (n_12835555, n_1845453989) = n1773789275.split(0.6123032f)
      n1552341417_               <- runSections(n_12835555, List((10, 0.7348425f)), "n_12835555", enableOutput)
      (n1552341417, _)           = n1552341417_
      n470165866_ <- runSections(
                      n_1845453989,
                      List((10, 0.54476756f)),
                      "n_1845453989",
                      enableOutput
                    )
      (n470165866, _)           = n470165866_
      n_1739841122              = n1552341417 >|< n470165866
      n_23410212                = n_1739841122 >|< n221340169
      (n716266525, n285850228)  = n_23410212.split(0.38010907f)
      n2118929613_              <- runSections(n716266525, List((3, 0.085235894f)), "n716266525", enableOutput)
      (n2118929613, _)          = n2118929613_
      n_328185564_              <- runSections(n285850228, List((10, 0.84769547f)), "n285850228", enableOutput)
      (n_328185564, _)          = n_328185564_
      n_1099161676              = n_328185564 >|< n2118929613
      (n_1858996816, n80197349) = n_1099161676.split(0.24985152f)
      n110964627_ <- runSections(
                      n_1858996816,
                      List((14, 0.44141734f)),
                      "n_1858996816",
                      enableOutput
                    )
      (n110964627, _)             = n110964627_
      n_1585794645_               <- runSections(n80197349, List((13, 0.16910058f)), "n80197349", enableOutput)
      (n_1585794645, _)           = n_1585794645_
      n1008581210                 = n_1585794645 >|< n110964627
      (n_422046842, n_1538497912) = n1008581210.split(0.11095542f)
      n_95554015_ <- runSections(
                      n_422046842,
                      List((15, 0.38963544f)),
                      "n_422046842",
                      enableOutput
                    )
      (n_95554015, _) = n_95554015_
      n1042570617_ <- runSections(
                       n_1538497912,
                       List((10, 0.23109722f)),
                       "n_1538497912",
                       enableOutput
                     )
      (n1042570617, _)          = n1042570617_
      n45761784                 = n1042570617 >|< n_95554015
      (n1464488337, n826889543) = n45761784.split(0.23212159f)
      n_168127302_ <- runSections(
                       n1464488337,
                       List((10, 0.8466194f)),
                       "n1464488337",
                       enableOutput
                     )
      (n_168127302, _)            = n_168127302_
      n_188358953_                <- runSections(n826889543, List((2, 0.8328306f)), "n826889543", enableOutput)
      (n_188358953, _)            = n_188358953_
      (n_1391966993, n1671848291) = n_168127302.split(0.25164324f)
      n1190750483_ <- runSections(
                       n_1391966993,
                       List((13, 0.32787257f)),
                       "n_1391966993",
                       enableOutput
                     )
      (n1190750483, _)           = n1190750483_
      n995028883_                <- runSections(n1671848291, List((8, 0.77417606f)), "n1671848291", enableOutput)
      (n995028883, _)            = n995028883_
      (n_1269818380, n335670858) = n_188358953.split(0.08835584f)
      r                          <- runSections(n_1269818380, List((8, 0.07683784f)), "n_1269818380", enableOutput)
    } yield r
  }
}
