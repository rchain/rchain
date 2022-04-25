package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits.toFoldableOps
import cats.{Eval => _}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Substitute.{charge => _}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.BitSet
import scala.concurrent.duration.{Duration, FiniteDuration}

class RhoPrimPerfTest
    extends FlatSpec
    with PropertyChecks
    with Matchers
    with PersistentStoreTester {

  def durationRaw[A](block: => Task[A]): Task[(A, FiniteDuration)] =
    for {
      t0 <- Sync[Task].delay(System.nanoTime)
      a  <- block
      t1 = System.nanoTime
      m  = Duration.fromNanos(t1 - t0)
    } yield (a, m)

  def nsTime[A](block: Task[A]): Task[(A, Long)] =
    durationRaw(block).map(x => (x._1, x._2.toMillis))

  def constructMap(size: Long) = {
    val initMap = SortedParMap(Seq.empty)
    (1L to size).foldLeft(initMap) { (map, j) =>
      val value: Par = GInt(j)
      map + (value -> value)
    }
  }

  def constructSet(size: Long) = {
    val initSet = SortedParHashSet(Seq.empty)
    (1L to size).foldLeft(initSet) { (set, j) =>
      val value: Par = GInt(j)
      set + value
    }
  }

  def constructList(size: Long) = {
    val initSeq: Seq[Par] = Seq.empty
    val seq = (1L to size).foldLeft(initSeq) { (seq, j) =>
      val value: Par = GInt(j)
      seq :+ value
    }
    EList(seq)
  }

  "SortedParMap appendMap test" should "executed with monix" in {
    val stepSize = 100L
    for (i <- 1 to 5) {
      val size = stepSize * i
      constructMap(size) // warmUp
      val t0 = System.nanoTime
      constructMap(size)
      val t1 = System.nanoTime
      val m  = Duration.fromNanos(t1 - t0).toMillis
      println(s"${size}, ${m}")
    }
  }

  "Reducer without loop appendMap test" should "executed with monix" in {
    def appendMapWithoutLoop(
        reducer: DebruijnInterpreter[Task],
        size: Long
    ) = {
      implicit val env: Env[Par] = Env.makeEnv[Par]()

      val initMap = EMapBody(ParMap(List[(Par, Par)]()))

      (1L to size).toList.foldM(initMap) {
        case (map, j) =>
          for {
            expr   <- Sync[Task].delay(EMethodBody(EMethod("set", map, List(GInt(j), GInt(j)))))
            resPar <- reducer.evalExpr(expr)
            r = resPar.exprs.head match {
              case Expr(newMap: EMapBody) =>
                newMap
              case _ => initMap
            }
          } yield r
      }
    }

    val stepSize = 100L
    val _ = withTestSpace {
      case TestFixture(_, reducer) =>
        def singleExperience(size: Long) =
          for {
            _         <- appendMapWithoutLoop(reducer, size)
            temp      <- nsTime(appendMapWithoutLoop(reducer, size))
            (_, time) = temp
          } yield time

        for (i <- 1 to 5) {
          val size = stepSize * i
          val m    = singleExperience(size).runSyncUnsafe()
          println(s"${size}, ${m}")
        }
    }
  }

  implicit val rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])

  "reducer with loop appendMap test" should "executed with monix" in {
    def appendMapWithLoop(
        reducer: DebruijnInterpreter[Task],
        size: Long
    ) = {
      implicit val env: Env[Par] = Env.makeEnv[Par]()

      val splitRand0 = rand.splitByte(0)
      val splitRand1 = rand.splitByte(1)

      // @{"loop"}!((x0 - 1), (x1).set(x0,x0))
      def loopSend = Send(
        chan = GString("loop"),
        data = List(
          EMinus(EVar(BoundVar(1)), GInt(1L)),
          EMethodBody(
            EMethod("set", EVar(BoundVar(0)), List(EVar(BoundVar(1)), EVar(BoundVar(1))))
          )
        ),
        locallyFree = BitSet()
      )

      // match (x0 == 0) {
      //   true => { Nil }
      //   false => { loopSend }
      //}
      def matchPar() = Match(
        target = EEq(EVar(BoundVar(1)), GInt(0L)),
        cases = List(
          MatchCase(pattern = GBool(true), source = Par()),
          MatchCase(pattern = GBool(false), source = loopSend)
        ),
        locallyFree = BitSet()
      )

      // for( @{x0}, @{x1} <= @{"loop"} ) { matchPar }
      def persistentReceive = Receive(
        binds = Seq(
          ReceiveBind(
            Seq(EVarBody(EVar(FreeVar(0))), EVarBody(EVar(FreeVar(1)))),
            GString("loop"),
            freeCount = 2
          )
        ),
        body = matchPar(),
        persistent = true,
        bindCount = 2,
        locallyFree = BitSet()
      )

      // @{"loop"}!(size, {})
      val initMap = EMapBody(ParMap(List[(Par, Par)]()))
      def startSend(size: Long) = Send(
        chan = GString("loop"),
        data = List(GInt(size), initMap),
        locallyFree = BitSet()
      )

      /*
              @{"loop"}!(size, {}) |
              for( @{x0}, @{x1} <= @{"loop"} ) {
                match (x0 == 0) {
                  true => {
                    Nil
                  }
                  false => {
                    @{"loop"}!((x0 - 1), (x1).set(x0,x0))
                  }
                }
              }
       */
      for {
        _ <- reducer.eval(startSend(size))(env, splitRand0)
        _ <- reducer.eval(persistentReceive)(env, splitRand1)
      } yield ()
    }

    val stepSize = 100L
    val _ = withTestSpace {
      case TestFixture(_, reducer) =>
        def singleExperience(size: Long) =
          for {
            _         <- appendMapWithLoop(reducer, size) // Warm up
            temp      <- nsTime(appendMapWithLoop(reducer, size))
            (_, time) = temp
          } yield time

        for (i <- 1 to 5) {
          val size = stepSize * i
          val m    = singleExperience(size).runSyncUnsafe()
          println(s"${size}, ${m}")
        }
    }
  }

  def time[A](block: => A): (A, Long) = {
    val t0 = System.nanoTime
    val a  = block
    val t1 = System.nanoTime
    val m  = Duration.fromNanos(t1 - t0).toMillis
    (a, m)
  }

  "List tests" should "executed with monix" in {
    implicit val env: Env[Par] = Env.makeEnv[Par]()

    val splitRand0 = rand.splitByte(0)
    val stepSize   = 100L
    val _ = withTestSpace {
      case TestFixture(_, reducer) =>
        def send(list: Par) = Send(
          chan = GString("loop"),
          data = List(list),
          locallyFree = BitSet()
        )
        def experiment(list: Par) =
          for {
            _          <- reducer.evalExpr(list) // WarmUp
            temp       <- nsTime(reducer.evalExpr(list))
            (_, tEval) = temp

            _                <- Substitute[Task, Par].substitute(list)(0, env) // WarmUp
            temp             <- nsTime(Substitute[Task, Par].substitute(list)(0, env))
            (_, tSubstitute) = temp

            _ <- reducer.produce(GString("loop"), ListParWithRandom(List(list), splitRand0), false) // WarmUp
            temp <- nsTime(
                     reducer
                       .produce(GString("loop"), ListParWithRandom(List(list), splitRand0), false)
                   )
            (_, tProduce) = temp

            _          <- reducer.eval(send(list))(env, splitRand0) // WarmUp
            temp       <- nsTime(reducer.eval(send(list))(env, splitRand0))
            (_, tSend) = temp

            appendFun   = EPlusPlusBody(EPlusPlus(list, EListBody(EList(Seq(GInt(999L))))))
            _           <- reducer.evalExpr(appendFun) // WarmUp
            temp        <- nsTime(reducer.evalExpr(appendFun))
            (_, tSlice) = temp

          } yield (tEval, tSubstitute, tProduce, tSend, tSlice)
        println(s"size, ListConstruct, ParConstruct, Eval, Substitute, Produce, Send, Append")
        for (i <- 1 to 10) {
          val size = stepSize * i

          { val _ = constructList(size) }
          val (list, tListConstruct) = time(constructList(size))

          { val _ = EListBody(list) }
          val (reducerList, tParConstruct) = time(EListBody(list))

          val (tEval, tSubstitute, tProduce, tSend, tAppend) =
            experiment(reducerList).runSyncUnsafe()
          println(
            s"${size}, ${tListConstruct}, ${tParConstruct}, ${tEval}, ${tSubstitute}, ${tProduce}, ${tSend}, ${tAppend}"
          )
        }
    }
  }

  "Set tests" should "executed with monix" in {
    implicit val env: Env[Par] = Env.makeEnv[Par]()

    val splitRand0 = rand.splitByte(0)
    val stepSize   = 100L
    val _ = withTestSpace {
      case TestFixture(_, reducer) =>
        def send(set: Par) = Send(
          chan = GString("loop"),
          data = List(set),
          locallyFree = BitSet()
        )
        def experiment(set: Par) =
          for {
            _          <- reducer.evalExpr(set) // WarmUp
            temp       <- nsTime(reducer.evalExpr(set))
            (_, tEval) = temp

            _                <- Substitute[Task, Par].substitute(set)(0, env) // WarmUp
            temp             <- nsTime(Substitute[Task, Par].substitute(set)(0, env))
            (_, tSubstitute) = temp

            _ <- reducer.produce(GString("loop"), ListParWithRandom(List(set), splitRand0), false) // WarmUp
            temp <- nsTime(
                     reducer
                       .produce(GString("loop"), ListParWithRandom(List(set), splitRand0), false)
                   )
            (_, tProduce) = temp

            _          <- reducer.eval(send(set))(env, splitRand0) // WarmUp
            temp       <- nsTime(reducer.eval(send(set))(env, splitRand0))
            (_, tSend) = temp

            appendFun    = EMethodBody(EMethod("add", set, List(GInt(9999L))))
            _            <- reducer.evalExpr(appendFun) // WarmUp
            temp         <- nsTime(reducer.evalExpr(appendFun))
            (_, tAppend) = temp

          } yield (tEval, tSubstitute, tProduce, tSend, tAppend)
        println(s"size, SetConstruct, ParConstruct, Eval, Substitute, Produce, Send, Append")
        for (i <- 1 to 10) {
          val size = stepSize * i

          { val _ = constructSet(size) }
          val (sortedSet, tSetConstruct) = time(constructSet(size))

          { val _ = ESetBody(ParSet(sortedSet.sortedPars)) }
          val (reducerSet, tParConstruct) = time(ESetBody(ParSet(sortedSet.sortedPars)))

          val (tEval, tSubstitute, tProduce, tSend, tAppend) =
            experiment(reducerSet).runSyncUnsafe()
          println(
            s"${size}, ${tSetConstruct}, ${tParConstruct}, ${tEval}, ${tSubstitute}, ${tProduce}, ${tSend}, ${tAppend}"
          )
        }
    }
  }

  "Map tests" should "executed with monix" in {
    implicit val env: Env[Par] = Env.makeEnv[Par]()

    val splitRand0 = rand.splitByte(0)
    val stepSize   = 100L
    val _ = withTestSpace {
      case TestFixture(_, reducer) =>
        def send(map: Par) = Send(
          chan = GString("loop"),
          data = List(map),
          locallyFree = BitSet()
        )
        def sendMap(map: Par) =
          for {
            _          <- reducer.evalExpr(map) // WarmUp
            temp       <- nsTime(reducer.evalExpr(map))
            (_, tEval) = temp

            _                      <- Substitute[Task, Par].substituteNoSort(map)(0, env) // WarmUp
            temp                   <- nsTime(Substitute[Task, Par].substituteNoSort(map)(0, env))
            (_, tSubstituteNoSort) = temp

            _                <- Substitute[Task, Par].substitute(map)(0, env) // WarmUp
            temp             <- nsTime(Substitute[Task, Par].substitute(map)(0, env))
            (_, tSubstitute) = temp

            _ <- reducer.produce(GString("loop"), ListParWithRandom(List(map), splitRand0), false) // WarmUp
            temp <- nsTime(
                     reducer
                       .produce(GString("loop"), ListParWithRandom(List(map), splitRand0), false)
                   )
            (_, tProduce) = temp

            _          <- reducer.eval(send(map))(env, splitRand0) // WarmUp
            temp       <- nsTime(reducer.eval(send(map))(env, splitRand0))
            (_, tSend) = temp

            appendFun    = EMethodBody(EMethod("set", map, List(GInt(9999L), GInt(9999L))))
            _            <- reducer.evalExpr(appendFun) // WarmUp
            temp         <- nsTime(reducer.evalExpr(appendFun))
            (_, tAppend) = temp

          } yield (tEval, tSubstituteNoSort, tSubstitute, tProduce, tSend, tAppend)
        println(
          s"size, MapConstruct, ParConstruct, Eval, SubstituteNoSort, Substitute, Produce, Send, Append"
        )
        for (i <- 1 to 10) {
          val size = stepSize * i

          { val _ = constructMap(size) }
          val (sortedMap, tMapConstruct) = time(constructMap(size))

          { val _ = EMapBody(ParMap(sortedMap)) }
          val (reducerMap, tParConstruct) = time(EMapBody(ParMap(sortedMap)))

          val (tEval, tSubstituteNoSort, tSubstitute, tProduce, tSend, tAppend) =
            sendMap(reducerMap).runSyncUnsafe()
          println(
            s"${size}, ${tMapConstruct}, ${tParConstruct}, ${tEval}, ${tSubstituteNoSort}, ${tSubstitute}, ${tProduce}, ${tSend}, ${tAppend}"
          )
        }
    }
  }

  "Produce test" should "executed with monix" in {
    val splitRand0 = rand.splitByte(0)
    val stepSize   = 100L
    val _ = withTestSpace {
      case TestFixture(_, reducer) =>
        def produceMap(map: Par) =
          for {
            _ <- reducer.produce(GString("loop"), ListParWithRandom(List(map), splitRand0), false) // WarmUp
            temp <- nsTime(
                     reducer
                       .produce(GString("loop"), ListParWithRandom(List(map), splitRand0), false)
                   )
            (_, tProduce) = temp
          } yield (tProduce)
        println(s"size, Produce")
        for (i <- 1 to 10) {
          val size       = stepSize * i
          val reducerMap = EMapBody(ParMap(constructMap(size)))
          val tProduce   = produceMap(reducerMap).runSyncUnsafe()
          println(s"${size}, ${tProduce}")
        }
    }
  }
}
