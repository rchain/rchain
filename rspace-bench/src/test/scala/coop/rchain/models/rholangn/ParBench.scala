package coop.rchain.models.rholangn

import coop.rchain.models.rholangn.parmanager.{Manager, Serialization}
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

@Fork(value = 1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OperationsPerInvocation(value = 100)
@State(Scope.Benchmark)
class ParBench {

  @tailrec
  final def createNestedPar(n: Int, par: ParN = GIntN(0)): ParN =
    if (n == 0) par
    else createNestedPar(n - 1, EListN(par))

  final def createParProc(n: Int): ParN = {
    val elSize     = 33
    def el(i: Int) = EListN(Seq.fill(elSize)(GIntN(i.toLong)))
    val seq        = Seq.tabulate(n)(el)
    ParN.makeParProc(seq)
  }

  final def appendTest(n: Int): ParN = {
    val elSize     = 33
    def el(i: Int) = EListN(Seq.fill(elSize)(GIntN(i.toLong)))

    val seq = Seq.tabulate(n)(el)
    seq.foldLeft(NilN: ParN) { (acc, p) =>
      ParN.combine(acc, p)
    }
  }
  val nestedSize: Int             = 500
  var nestedPar: ParN             = _
  var nestedAnotherPar: ParN      = _
  var nestedParSData: Array[Byte] = _

  val parProcSize: Int          = 500
  var parProc: ParN             = _
  var parProcAnother: ParN      = _
  var parProcSData: Array[Byte] = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    nestedPar = createNestedPar(nestedSize)
    nestedAnotherPar = createNestedPar(nestedSize)
    nestedParSData = nestedPar.serialized.value

    parProc = createParProc(parProcSize)
    parProcAnother = createParProc(parProcSize)
    parProcSData = parProc.serialized.value
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def nestedCreation(): Unit = {
    val _ = createNestedPar(nestedSize)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def nestedSerialization(): Unit = {
    val _ = nestedPar.serialized.value
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def nestedDeserialization(): Unit = {
    val _ = Manager.protoDeserialize(nestedParSData)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def nestedSerializedSize(): Unit = {
    val _ = nestedPar.serializedSize.value
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def nestedHash(): Unit = {
    val _ = nestedPar.rhoHash
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def nestedEqual(): Unit = {
    val _ = nestedPar.equals(nestedAnotherPar)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def nestedAdd(): Unit =
    ParProcN(Seq(nestedPar, GIntN(0)))

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def parProcCreation(): Unit = {
    val _ = createParProc(parProcSize)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def parProcSerialization(): Unit = {
    val _ = parProc.serialized.value
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def parProcDeserialization(): Unit = {
    val _ = Manager.protoDeserialize(parProcSData)
  }
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def parProcSerializedSize(): Unit = {
    val _ = parProc.serializedSize.value
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def parProcHash(): Unit = {
    val _ = parProc.rhoHash
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def parProcEqual(): Unit = {
    val _ = parProc.equals(parProcAnother)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def parProcAdd(): Unit = {
    val _ = parProc match {
      case proc: ParProcN => ParN.combine(proc, GIntN(0))
      case _              => assert(false)
    }
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def manyAppends(): Unit = {
    val _ = appendTest(parProcSize)
  }
}
