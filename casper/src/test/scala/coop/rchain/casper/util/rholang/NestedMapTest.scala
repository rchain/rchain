package coop.rchain.casper.util.rholang

import cats.effect.Sync
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.rholang.NestedMapTest._
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task
import org.scalatest.FlatSpec

import java.nio.file
import java.nio.file.Paths
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.io.{Directory, Path}

class NestedMapTest extends FlatSpec {

  "creating test" should "be performed" in {
    val strMap = createNestedMap()
    val deploy = s"""
                    |    $strMap
                    |""".stripMargin
    clearLmdb
    val time = evaluateAndDoCheckpoint(deploy)
    println(s"Map [ ${prettyMapSize()}], time(ms)")
    println("Creating map, " ++ time.toString)
  }

  "send test" should "be performed" in {
    val strMap = createNestedMap()
    val deploy = s"""
            |    @"storage"!($strMap)
            |""".stripMargin
    clearLmdb
    val time = evaluateAndDoCheckpoint(deploy)
    println("Produce, " ++ time.toString)
  }

  "map" should "be print" ignore {
    val deploy =
      """
        |new print(`rho:io:stdout`) in {
        |      for (@map <- @"storage") {
        |        print!(map) | 
        |        @"storage"!(map)
        |      }
        |}
        |""".stripMargin
    val _ = evaluateAndDoCheckpoint(deploy)
  }

  "receive test" should "be performed" in {
    val deploy =
      """
        |      for (@map <- @"storage") {
        |        Nil
        |      }
        |""".stripMargin
    val time = evaluateAndDoCheckpoint(deploy)
    println("Consume, " ++ time.toString)
  }

  "append test" should "be performed" in {
    val strMap = createNestedMap()
    val deploy = s"""
                    |$strMap.set("newKey", "newValue")
                    |""".stripMargin
    val time   = evaluateAndDoCheckpoint(deploy)
    println("Append, " ++ time.toString)
  }

  "concat test" should "be performed" in {
    val strMap = createNestedMap()
    val deploy = s"""
                    |$strMap ++ {"newKey", "newValue"}
                    |""".stripMargin
    val time   = evaluateAndDoCheckpoint(deploy)
    println("Concatenation, " ++ time.toString)
  }

  "replace test" should "be performed" in {
    val strMap = createNestedMap()
    val deploy = s"""
                    |$strMap.set("1", "newValue")
                    |""".stripMargin
    val time   = evaluateAndDoCheckpoint(deploy)
    println("Replace, " ++ time.toString)
  }

  "delete test" should "be performed" in {
    val strMap = createNestedMap()
    val deploy = s"""
                    |$strMap.delete("1")
                    |""".stripMargin
    val time   = evaluateAndDoCheckpoint(deploy)
    println("Delete, " ++ time.toString)
  }

  "match1 test" should "be performed" in {
    val time = matchTest(1)
    println("Match1, " ++ time.toString)
  }

  "match5 test" should "be performed" in {
    val time = matchTest(5)
    println("Match5, " ++ time.toString)
  }
  "matching replace1" should "be performed" ignore {
    val time = replaceTest(1)
    println("matching replace1, " ++ time.toString)
  }

  "matching replace5" should "be performed" ignore {
    val time = replaceTest(5)
    println("matching replace5, " ++ time.toString)
  }

  "matching append1" should "be performed" ignore {
    val time = replaceTest(1, append = true)
    println("matching append1, " ++ time.toString)
  }

  "matching append5" should "be performed" ignore {
    val time = replaceTest(5, append = true)
    println("matching append5, " ++ time.toString)
  }

  def matchTest(
      numberRecords: Int,
      startLevel: Int = 1,
      settings: Seq[Int] = mapSettings,
      stopLevel: Int = numberLevels
  ): Long = {
    val strMap = createNestedMap(settings)
    val terms =
      createMatchTerms(numberRecords, startLevel, stopLevel, valueNaming = true, restNaming = false)
    val values = createValues(numberRecords)
    val deploy = s"""
                    |new print(`rho:io:stdout`) in {
                    |    match $strMap {
                    |        $terms => $values  //for validate add print!(...)
                    |        _ => print!("No match!")
                    |    }
                    |}
                    |""".stripMargin
    evaluateAndDoCheckpoint(deploy)
  }

  def replaceTest(
      numberRecords: Int,
      append: Boolean = false,
      startLevel: Int = 1,
      settings: Seq[Int] = mapSettings,
      stopLevel: Int = numberLevels
  ): Long = {
    val strMap = createNestedMap(settings)
    val terms =
      createMatchTerms(numberRecords, startLevel, stopLevel, valueNaming = false, restNaming = true)
    val replaceActions = createMatchActions(numberRecords, startLevel, stopLevel, append)
    val deploy         = s"""
                            |new print(`rho:io:stdout`) in {
                            |    match $strMap {
                            |        $terms => $replaceActions //for validate add print!(...)
                            |        _ => print!("No match!")
                            |    }
                            |}
                            |""".stripMargin
    evaluateAndDoCheckpoint(deploy)
  }

  "matchN test" should "be performed" ignore {
    val mapSettings    = Seq(100, 100)
    val numExperiments = 10
    val step           = 10
    val startLevel     = 1
    println(s"Matching test: Map [ ${prettyMapSize(mapSettings)} ] startLevel = $startLevel")
    println(s"numberRecords, time(ms)")
    for (i <- 1 to numExperiments) {
      val numberRecords = i * step
      val time          = matchTest(numberRecords, startLevel, mapSettings, mapSettings.size)
      println(s"$numberRecords, ${time.toString}")
    }
  }

  "matching replace (append) test" should "be performed" in {
    val appendTest     = false // if set - append test
    val mapSettings    = Seq(2500, 1)
    val numExperiments = 10
    val step           = 5
    val startLevel     = 1
    val str            = if (appendTest) "append" else "replace"
    println(
      s"Matching $str test: Map [ ${prettyMapSize(mapSettings)} ] startLevel = $startLevel"
    )
    println(s"numberRecords, time(ms)")
    for (i <- 1 to numExperiments) {
      val numberRecords = i * step
      val time          = replaceTest(numberRecords, appendTest, startLevel, mapSettings, mapSettings.size)
      println(s"$numberRecords, ${time.toString}")
    }
  }

  "nested map" should "be print" ignore {
    val str = {
      createNestedMap(Seq(3, 3))
    }
    println(str)
  }

  "match terms" should "be print" ignore {
    val str = createMatchTerms(
      number = 3,
      startLevel = 1,
      stopLevel = 3,
      valueNaming = true,
      restNaming = true
    )
    println(str)
  }

  "match actions" should "be print" ignore {
    val str = createMatchActions(number = 3, startLevel = 1, stopLevel = 3, appendNewKey = false)
    println(str)
  }
}

object NestedMapTest {
  import monix.execution.Scheduler.Implicits.global

  /** Automatic or manual init map creation
    * True - automatic creation of init data
    * False - manual creation of init data */
  val automatic: Boolean = false

  /** Sequence with size of map in each level */
  val mapSettings: Seq[Int] = if (automatic) {
    val numberLevels = 3 // Number of levels in nested Map
    val mapSize      = 5 // Number of elements in each Map
    Seq.fill(numberLevels)(mapSize)
  } else Seq(20, 20, 20)

  val numberLevels: Int = mapSettings.size

  def prettyMapSize(settings: Seq[Int] = mapSettings): String = settings.foldLeft("") {
    (acc, curSize) =>
      acc ++ curSize.toString ++ " "
  }

  val workPath: file.Path = Paths.get("/git/lmdb_data")
  val workDir: Directory  = Directory(Path(workPath.toFile))

  val workPathW: file.Path = Paths.get("/git/lmdb_data_W")    // WarmUp
  val workDirW: Directory  = Directory(Path(workPath.toFile)) // WarmUp

  def clearLmdb: Boolean = {
    workDir.deleteRecursively
    workDirW.deleteRecursively
  }

  def quotes(x: Any): String = s""""$x""""

  def time[A](block: => A): (A, Long) = {
    val t0 = System.nanoTime
    val a  = block
    val t1 = System.nanoTime
    val m  = Duration.fromNanos(t1 - t0).toMillis
    (a, m)
  }

  def durationRaw[A](block: => Task[A]): Task[(A, FiniteDuration)] =
    for {
      t0 <- Sync[Task].delay(System.nanoTime)
      a  <- block
      t1 = System.nanoTime
      m  = Duration.fromNanos(t1 - t0)
    } yield (a, m)

  def timeF[A](block: Task[A]): Task[(A, Long)] =
    durationRaw(block).map(x => (x._1, x._2.toMillis))

  def evaluateAndDoCheckpoint(deploy: String): Long = {
    implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val logger: Log[Task]         = Log.log[Task]
    implicit val random: Blake2b512Random =
      Tools.rng(Blake2b256Hash.create(Array[Byte](1)).toByteString.toByteArray)

    def run: Task[Long] =
      for {
        kvmW          <- RNodeKeyValueStoreManager[Task](workPathW)
        kvm           <- RNodeKeyValueStoreManager[Task](workPath)
        storeW        <- kvmW.rSpaceStores
        store         <- kvm.rSpaceStores
        runtimeW      <- RhoRuntime.createRuntime(storeW, Genesis.NonNegativeMergeableTagName)
        runtime       <- RhoRuntime.createRuntime(store, Genesis.NonNegativeMergeableTagName)
        _             <- timeF(runtimeW.evaluate(deploy, Cost.UNSAFE_MAX, Map.empty))
        resAndTime    <- timeF(runtime.evaluate(deploy, Cost.UNSAFE_MAX, Map.empty))
        (_, evalTime) = resAndTime
        _             <- runtimeW.createCheckpoint
        _             <- runtime.createCheckpoint
      } yield evalTime

    run.unsafeRunSync
  }

  /**
    * Create nested Map
    * @param settings Sequence with size of map in each level
    * @return String with Rholang code
    * createNestedMap(Seq(3, 3, 3)) =
    * 	{"1":{"1":{"1":"data1", "2":"data2", "3":"data3"}, "2":{"1":"data4", "2":"data5", "3":"data6"}, "3":{"1":"data7", "2":"data8", "3":"data9"}},
    * 	 "2":{"1":{"1":"data10", "2":"data11", "3":"data12"}, "2":{"1":"data13", "2":"data14", "3":"data15"}, "3":{"1":"data16", "2":"data17", "3":"data18"}},
    * 	 "3":{"1":{"1":"data19", "2":"data20", "3":"data21"}, "2":{"1":"data22", "2":"data23", "3":"data24"}, "3":{"1":"data25", "2":"data26", "3":"data27"}}}

    * createNestedMap(Seq(2, 2)) =
    * 	{"1":{"1":"data1", "2":"data2"},
    *    "2":{"1":"data3", "2":"data4"}}
    */
  def createNestedMap(settings: Seq[Int] = mapSettings): String = {
    val numberLevelsLocal: Int = settings.size
    def loop(level: Int, firstElementIndex: Int): String = {
      val mapSize = settings(level - 1) // Size of map in this level

      def makeValue(index: Int): String =
        if (level >= numberLevelsLocal) // Bottom level
          quotes(s"data${firstElementIndex + (index - 1)}")
        else { // Top or intermediate level
          def mulSizes = settings.takeRight(numberLevelsLocal - level).product

          val offset: Int = (index - 1) * mulSizes
          loop(level + 1, firstElementIndex + offset)
        }

      def comma(index: Int) = if (index < mapSize) ", " else ""

      val strSeq =
        for (i <- 1 to mapSize) yield s"${quotes(i)}:${makeValue(i)}" ++ comma(i)
      val concatStr = strSeq.foldLeft("")(_ ++ _)
      s"{$concatStr}"
    }
    loop(1, 1)
  }

  /**
    * Create match conditions, e.g:
    * createMatchTerms(number = 3, startLevel = 1, stopLevel = 3, valueNaming = true, restNaming = false) =
    *    {"1" : {"1" : {"1" : v1 ... _} ... _},
    *     "2" : {"1" : {"1" : v2 ... _} ... _},
    *     "3" : {"1" : {"1" : v3 ... _} ... _} ... _}

    * createMatchTerms(number = 3, startLevel = 1, stopLevel = 2, valueNaming = false, restNaming = true) =
    * 	{"1" : {"1" : _ ... r1_2},
    *  	 "2" : {"1" : _ ... r2_2},
    *  	 "3" : {"1" : _ ... r3_2} ... r1_1}

    * createMatchTerms(number = 2, startLevel = 2, stopLevel = 3, valueNaming = false, restNaming = true) =
    * 	{"1" : {"1" : {"1" : _ ... r1_3},
    * 	 "2" : {"1" : _ ... r2_3} ... r1_2} ... r1_1}

    * @param number Number of terms
    * @param startLevel Nested map level where does the match start
    * @param stopLevel Nested map level before which the match takes place
    * @param valueNaming If false to using "_" instead of v1, v2, ...
    * @param restNaming If false to using "_" instead of r11, r12, ...
    * @return String with terms.
    */
  def createMatchTerms(
      number: Int,
      startLevel: Int,
      stopLevel: Int,
      valueNaming: Boolean,
      restNaming: Boolean
  ): String = {
    def collectTerms(level: Int) = (1 to number).foldLeft("") { (acc, i) =>
      val comma = if (i != number) ", " else ""
      acc ++ loop(i, level, isMap = false) ++ comma
    }

    def loop(idx: Int, level: Int, isMap: Boolean): String = {
      def value                         = if (valueNaming) s"v$idx" else "_"
      def maybeValue                    = if (level < stopLevel) s"${loop(idx, level + 1, isMap = true)}" else value
      def rest                          = if (restNaming) s"r${idx}_$level" else "_"
      def addMapAttribute(term: String) = s"{$term ... $rest}"
      (level, isMap) match {
        case (`startLevel`, true) => addMapAttribute(collectTerms(level))
        case (_, true)            => addMapAttribute(s"${quotes(1)} : $maybeValue")
        case (_, false)           => s"${quotes(idx)} : $maybeValue"
      }
    }
    loop(1, 1, isMap = true)
  }

  /**
    * Create match actions, e.g:
    * createMatchActions(number = 3, startLevel = 1, stopLevel = 3, appendNewKey = false) =
    * 	r1_1 ++ ({"1" : r1_2 ++ {"1" : r1_3 ++ {"1" : "newData1"}}} ++
    *     	     {"2" : r2_2 ++ {"1" : r2_3 ++ {"1" : "newData2"}}} ++
    *         	 {"3" : r3_2 ++ {"1" : r3_3 ++ {"1" : "newData3"}}})

    * createMatchActions(number = 3, startLevel = 2, stopLevel = 3, appendNewKey = true) =
    * 	r1_1 ++ {"1" : r1_2 ++ ({"newKey1" : r1_3 ++ {"1" : "newData1"}} ++
    * 					            		{"newKey2" : r2_3 ++ {"1" : "newData2"}} ++
    * 							            {"newKey3" : r3_3 ++ {"1" : "newData3"}})}

    * createMatchActions(number = 2, startLevel = 1, stopLevel = 2, appendNewKey = false) =
    * 	r1_1 ++ ({"1" : r1_2 ++ {"1" : "newData1"}} ++
    *     			 {"2" : r2_2 ++ {"1" : "newData2"}})
    * @param number Number of actions
    * @param startLevel Nested map level where does the actions start
    * @param stopLevel Nested map level before which the actions take place
    * @param appendNewKey If set - will using new key for name
    * @return String with actions.
    */
  def createMatchActions(
      number: Int,
      startLevel: Int,
      stopLevel: Int,
      appendNewKey: Boolean
  ): String = {
    val startCollect = startLevel + 1
    def collectActions(level: Int) = (1 to number).foldLeft("") { (acc, i) =>
      val plusPlus = if (i != number) " ++ " else ""
      acc ++ loop(i, level, isMap = false) ++ plusPlus
    }

    def loop(idx: Int, level: Int, isMap: Boolean): String = {
      def concatRest(action: String) = s"r${idx}_$level ++ $action"
      def map                        = concatRest(loop(idx, level + 1, isMap = true))
      def value                      = quotes(s"newData$idx")
      def maybeValue                 = if (level < stopLevel + 1) map else value
      def keyPrefix                  = if (appendNewKey) "newKey" else ""
      (level, isMap) match {
        case (1, _)                 => maybeValue
        case (`startCollect`, true) => s"(${collectActions(level)})"
        case (_, false)             => s"{${quotes(keyPrefix ++ idx.toString)} : $maybeValue}"
        case _                      => s"{${quotes(1)} : $maybeValue}"
      }
    }
    loop(1, 1, isMap = true)
  }

  /**
    * Create string tuple with values: (v1, v2, ...)
    * @param number Number of values
    * @return
    */
  def createValues(number: Int): String = {
    val values = (1 to number).foldLeft("") { (acc, i) =>
      val comma = if (i == number) "" else ", "
      acc ++ s"v$i" ++ comma
    }
    s"($values)"
  }
}
