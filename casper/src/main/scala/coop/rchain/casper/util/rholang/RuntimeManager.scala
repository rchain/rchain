package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models._
import coop.rchain.rholang.interpreter.{ErrorLog, Reduce, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rspace.{trace, Blake2b256Hash, Checkpoint}
import monix.execution.Scheduler
import coop.rchain.rspace.internal.WaitingContinuation

import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}
import RuntimeManager.{DeployError, StateHash}
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.trace.Produce
import monix.eval.Task

import scala.collection.immutable

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class RuntimeManager private (val emptyStateHash: ByteString, runtimeContainer: SyncVar[Runtime]) {

  def captureResults(start: StateHash, term: Par, name: String = "__SCALA__")(
      implicit scheduler: Scheduler): Seq[Par] = {
    val runtime           = getResetRuntime(start)
    val deploy            = ProtoUtil.termDeploy(term)
    val costAccountingAlg = CostAccountingAlg.unsafe[Task](CostAccount.zero)
    val evalRes           = eval(deploy :: Nil, runtime.reducer, runtime.errorLog, costAccountingAlg)

    //TODO: Is better error handling needed here?
    val result: Seq[Datum[ListChannelWithRandom]] = evalRes.fold(
      {
        case (erroredTerm, errors, costAcc) =>
          Nil
      },
      costAcc => {
        val returnChannel = Channel(Quote(Par().copy(exprs = Seq(Expr(GString(name))))))
        runtime.space.getData(returnChannel)
      }
    )

    runtimeContainer.put(runtime)

    for {
      datum   <- result
      channel <- datum.a.channels
      par     <- channel.channelInstance.quote
    } yield par
  }

  def replayComputeState(log: trace.Log)(implicit scheduler: Scheduler)
    : (StateHash, Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])] = {
    (hash: StateHash, terms: Seq[Deploy]) =>
      {
        val runtime   = runtimeContainer.take()
        val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
        val riggedRuntime = Try(runtime.replaySpace.rig(blakeHash, log)) match {
          case Success(_) => runtime
          case Failure(ex) =>
            runtimeContainer.put(runtime)
            throw ex
        }
        val costAccountingAlg = CostAccountingAlg.unsafe[Task](CostAccount.zero)
        val error =
          eval(terms, riggedRuntime.replayReducer, riggedRuntime.errorLog, costAccountingAlg)
        val newCheckpoint = error.fold[Either[DeployError, (Checkpoint, Vector[DeployCost])]](
          Left(_),
          replayDeployCost =>
            Right((riggedRuntime.replaySpace.createCheckpoint(), replayDeployCost)))
        runtimeContainer.put(riggedRuntime)
        newCheckpoint
      }
  }

  def computeState(hash: StateHash, terms: Seq[Deploy])(
      implicit scheduler: Scheduler): Either[DeployError, (Checkpoint, Vector[DeployCost])] = {
    val resetRuntime: Runtime = getResetRuntime(hash)
    val costAccountingAlg     = CostAccountingAlg.unsafe[Task](CostAccount.zero)
    val error                 = eval(terms, resetRuntime.reducer, resetRuntime.errorLog, costAccountingAlg)
    val newCheckpoint = error.fold[Either[DeployError, (Checkpoint, Vector[DeployCost])]](
      Left(_),
      deployCosts => Right((resetRuntime.space.createCheckpoint(), deployCosts)))
    runtimeContainer.put(resetRuntime)
    newCheckpoint
  }

  def storageRepr(hash: StateHash): String = {
    val resetRuntime = getResetRuntime(hash)
    val result       = StoragePrinter.prettyPrint(resetRuntime.space.store)
    runtimeContainer.put(resetRuntime)
    result
  }

  def computeBonds(hash: StateHash): Seq[Bond] = {
    val resetRuntime = getResetRuntime(hash)
    // TODO: Switch to a read only name
    val bondsChannel     = Channel(Quote(Par().copy(exprs = Seq(Expr(GString("proofOfStake"))))))
    val bondsChannelData = resetRuntime.space.getData(bondsChannel)
    runtimeContainer.put(resetRuntime)
    toBondSeq(bondsChannelData)
  }

  private def toBondSeq(data: Seq[Datum[ListChannelWithRandom]]): Seq[Bond] = {
    assert(data.length == 1)
    val Datum(as: ListChannelWithRandom, _: Boolean, _: Produce) = data.head
    as.channels.head match {
      case Channel(Quote(p)) =>
        p.exprs.head.getEMapBody.ps.map {
          case (validator: Par, bond: Par) =>
            assert(validator.exprs.length == 1)
            assert(bond.exprs.length == 1)
            val validatorName = validator.exprs.head.getGString
            val stakeAmount   = bond.exprs.head.getGInt
            Bond(ByteString.copyFrom(Base16.decode(validatorName)), stakeAmount)
        }.toList
      case Channel(_) => throw new Error("Should never happen")
    }
  }

  def getData(hash: ByteString, channel: Channel): Seq[Par] = {
    val resetRuntime                              = getResetRuntime(hash)
    val result: Seq[Datum[ListChannelWithRandom]] = resetRuntime.space.getData(channel)
    runtimeContainer.put(resetRuntime)
    for {
      datum   <- result
      channel <- datum.a.channels
      par     <- channel.channelInstance.quote
    } yield par
  }

  def getContinuation(hash: ByteString,
                      channels: immutable.Seq[Channel]): Seq[(Seq[BindPattern], Par)] = {
    val resetRuntime = getResetRuntime(hash)
    val results: Seq[WaitingContinuation[BindPattern, TaggedContinuation]] =
      resetRuntime.space.getWaitingContinuations(channels)
    runtimeContainer.put(resetRuntime)
    for {
      result <- results.filter(_.continuation.taggedCont.isParBody)
    } yield (result.patterns, result.continuation.taggedCont.parBody.get.body)
  }

  private def getResetRuntime(hash: StateHash) = {
    val runtime   = runtimeContainer.take()
    val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
    Try(runtime.space.reset(blakeHash)) match {
      case Success(_) => runtime
      case Failure(ex) =>
        runtimeContainer.put(runtime)
        throw ex
    }
  }

  private def eval(terms: Seq[Deploy],
                   reducer: Reduce[Task],
                   errorLog: ErrorLog,
                   costAlg: CostAccountingAlg[Task],
                   accCost: Vector[DeployCost] = Vector.empty)(
      implicit scheduler: Scheduler): Either[DeployError, Vector[DeployCost]] =
    terms match {
      case deploy +: rest =>
        implicit val rand: Blake2b512Random = Blake2b512Random(
          DeployData.toByteArray(deploy.raw.get))
        implicit val costAlgebra = costAlg
        Try(reducer.inj(deploy.term.get).unsafeRunSync) match {
          case Success(_) =>
            val errors     = errorLog.readAndClearErrorVector()
            val cost       = CostAccount.toProto(costAlg.getCost().unsafeRunSync)
            val deployCost = DeployCost().withDeploy(deploy).withCost(cost)
            if (errors.isEmpty)
              eval(rest, reducer, errorLog, costAlg, accCost :+ deployCost)
            else
              Left((deploy, errors, accCost :+ deployCost))
          case Failure(ex) =>
            val otherErrors = errorLog.readAndClearErrorVector()
            val cost        = CostAccount.toProto(costAlg.getCost().unsafeRunSync)
            val deployCost  = DeployCost().withDeploy(deploy).withCost(cost)
            Left((deploy, otherErrors :+ ex, accCost :+ deployCost))
        }
      case Nil => Right(accCost)
    }
}

object RuntimeManager {
  type StateHash   = ByteString
  type DeployError = (Deploy, Vector[Throwable], Vector[DeployCost])

  def fromRuntime(active: Runtime): RuntimeManager = {
    active.space.clear()
    active.replaySpace.clear()
    val hash       = ByteString.copyFrom(active.space.createCheckpoint().root.bytes.toArray)
    val replayHash = ByteString.copyFrom(active.replaySpace.createCheckpoint().root.bytes.toArray)
    assert(hash == replayHash)
    val runtime = new SyncVar[Runtime]()
    runtime.put(active)

    new RuntimeManager(hash, runtime)
  }
}
