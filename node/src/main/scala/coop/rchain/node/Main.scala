package coop.rchain.node

import scala.collection.JavaConverters._
import scala.tools.jline.console._
import completer.StringsCompleter

import cats.implicits._

import coop.rchain.casper.util.comm._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.util.BondingUtil
import coop.rchain.comm._
import coop.rchain.node.configuration._
import coop.rchain.node.diagnostics.client.GrpcDiagnosticsService
import coop.rchain.node.effects._
import coop.rchain.shared._
import coop.rchain.shared.StringOps._

import monix.eval.Task
import monix.execution.Scheduler

object Nodes {
  def validator(n: Int, pk: String): Array[String] =
    s"""--grpc-port ${n}401 --grpc-port-internal ${n}402 run
       |--data-dir /Users/dzajkowski/tmp/commtest/p${n} --port ${n}400 --http-port ${n}480 --kademlia-port ${n}404
       |-b rnode://e7ef0862a965b2e26d2d29794d21874b88694afc@127.0.0.1?protocol=40400&discovery=40404
       |--validator-private-key $pk --host 127.0.0.1""".stripMargin
      .replaceAll("\n", " ")
      .split(" ")

  def ro(n: Int): Array[String] =
    s"""--grpc-port ${n}401 --grpc-port-internal ${n}402 run
       |--data-dir /Users/dzajkowski/tmp/commtest/p${n} --port ${n}400 --http-port ${n}480 --kademlia-port ${n}404
       |-b rnode://e7ef0862a965b2e26d2d29794d21874b88694afc@127.0.0.1?protocol=40400&discovery=40404
       |--host 127.0.0.1""".stripMargin
      .replaceAll("\n", " ")
      .split(" ")
}

//object Network {
//  def main(args: Array[String]): Unit = {
//    for {
//      b <- Task.delay()
//    }
//  }
//}

object A1 {
  def main(args: Array[String]): Unit =
    Main.main(
      "run -s --data-dir /Users/dzajkowski/tmp/commtest/p1 --validator-private-key 915f5ca82777dd9c11196068e4b853b4b1233174b40be093e583669cfe36bf2f --host 127.0.0.1"
        .split(" ")
    )
}

object B1 {
  def main(args: Array[String]): Unit =
    Main.main(
      Nodes.validator(41, "fff9aa47d5ff51605a4eceff8541fd52388a704b041132d8a14e6dd7fd5247f4")
    )
}

object B2 {
  def main(args: Array[String]): Unit =
    Main.main(
      Nodes.validator(42, "cf0790e56c5b7c1a8946c8fe7da1b68bf6eca8ed62426922f4f9551f89f8ff4d")
    )
}

object D1 {
  def main(args: Array[String]): Unit =
    (0 to 20).foreach { _ =>
      Actions.run(41, "phils")
    }
}

object D2 {
  def main(args: Array[String]): Unit =
    (0 to 5).foreach { _ =>
      Actions.run(42, "phils")
    }
}

object B3 {
  def main(args: Array[String]): Unit = {
    val x = Nodes.validator(43, "5e3026ccac36d30cf664f84fe7fc25ddba10d3bb434cd81bff7dcc942cb1fe14")
    Main.main(x)
  }
}

object S3 {
  def main(args: Array[String]): Unit =
    (0 to 1).foreach { _ =>
      Actions.show(43, 11)
    }
}

object C1 {
  def main(args: Array[String]): Unit =
    Main.main(Nodes.ro(60))
}

object C2 {
  def main(args: Array[String]): Unit =
    Main.main(Nodes.ro(61))
}

object Actions {
  val cs = Map(
    "phils" -> "/Users/dzajkowski/workspaces/ws_pyrofex/rchain/rholang/examples/tut-philosophers.rho",
    "hello" -> "/Users/dzajkowski/workspaces/ws_pyrofex/rchain/rholang/examples/tut-hello.rho",
    "hash"  -> "/Users/dzajkowski/workspaces/ws_pyrofex/rchain/rholang/examples/tut-hash-functions.rho",
    "reg"   -> "/Users/dzajkowski/workspaces/ws_pyrofex/rchain/rholang/examples/tut-registry.rho"
  )

  def deploy(contract: String, p: Int) = {
    val x =
      s"""--grpc-port ${p}401 --grpc-port-internal ${p}402 deploy --from 0x1 --phlo-limit 10000000000000 --phlo-price 1 --nonce 0 ${cs(
        contract
      )}""".stripMargin
        .replaceAll("\n", " ")
        .split(" ")
    Main.main(x)
  }

  def propose(p: Int) = {
    val x =
      s"""--grpc-port ${p}401 --grpc-port-internal ${p}402 propose""".stripMargin
        .replaceAll("\n", " ")
        .split(" ")
    Main.main(x)
  }

  def show(p: Int, s: Int = 1) = {
    val x =
      s"""--grpc-port ${p}401 --grpc-port-internal ${p}402 show-blocks --depth ${s}""".stripMargin
        .replaceAll("\n", " ")
        .split(" ")
    Main.main(x)
  }

  def run(p: Int, n: String) = {
    Actions.deploy(n, p)
    Actions.propose(p)
    Actions.show(p)
  }
}

object S2 {
  def main(args: Array[String]): Unit =
    (0 to 1).foreach { _ =>
      Actions.show(41, 11)
    }
}



object Main {

  private implicit val logSource: LogSource = LogSource(this.getClass)
  private implicit val log: Log[Task]       = effects.log

  def main(args: Array[String]): Unit = {

    implicit val scheduler: Scheduler = Scheduler.computation(
      Math.max(java.lang.Runtime.getRuntime.availableProcessors(), 2),
      "node-runner"
    )

    val exec: Task[Unit] =
      for {
        conf <- Configuration(args)
        _    <- Task.defer(mainProgram(conf))
      } yield ()

    exec.unsafeRunSync
  }

  private def mainProgram(conf: Configuration)(implicit scheduler: Scheduler): Task[Unit] = {
    implicit val replService: GrpcReplClient =
      new GrpcReplClient(
        conf.grpcServer.host,
        conf.grpcServer.portInternal,
        conf.server.maxMessageSize
      )
    implicit val diagnosticsService: GrpcDiagnosticsService =
      new diagnostics.client.GrpcDiagnosticsService(
        conf.grpcServer.host,
        conf.grpcServer.portInternal,
        conf.server.maxMessageSize
      )
    implicit val deployService: GrpcDeployService =
      new GrpcDeployService(
        conf.grpcServer.host,
        conf.grpcServer.portExternal,
        conf.server.maxMessageSize
      )

    implicit val time: Time[Task] = effects.time

    val program = conf.command match {
      case Eval(files) => new ReplRuntime().evalProgram[Task](files)
      case Repl        => new ReplRuntime().replProgram[Task].as(())
      case Diagnostics => diagnostics.client.Runtime.diagnosticsProgram[Task]
      case Deploy(address, phlo, phloPrice, nonce, location) =>
        DeployRuntime.deployFileProgram[Task](address, phlo, phloPrice, nonce, location)
      case DeployDemo        => DeployRuntime.deployDemoProgram[Task]
      case Propose           => DeployRuntime.propose[Task]()
      case ShowBlock(hash)   => DeployRuntime.showBlock[Task](hash)
      case ShowBlocks(depth) => DeployRuntime.showBlocks[Task](depth)
      case DataAtName(name)  => DeployRuntime.listenForDataAtName[Task](name)
      case ContAtName(names) => DeployRuntime.listenForContinuationAtName[Task](names)
      case Run               => nodeProgram(conf)
      case BondingDeployGen(bondKey, ethAddress, amount, secKey, pubKey) =>
        BondingUtil.writeIssuanceBasedRhoFiles[Task](bondKey, ethAddress, amount, secKey, pubKey)
      case FaucetBondingDeployGen(amount, sigAlgorithm, secKey, pubKey) =>
        BondingUtil.writeFaucetBasedRhoFiles[Task](amount, sigAlgorithm, secKey, pubKey)
      case _ => conf.printHelp()
    }

    program.doOnFinish(
      _ =>
        Task.delay {
          replService.close()
          diagnosticsService.close()
          deployService.close()
        }
    )
  }

  private def nodeProgram(conf: Configuration)(implicit scheduler: Scheduler): Task[Unit] = {
    log.info(VersionInfo.get)

    val node =
      for {
        runtime <- NodeRuntime(conf)
        _       <- runtime.main
      } yield ()

    node.value >>= {
      case Right(_) =>
        Task.unit
      case Left(CouldNotConnectToBootstrap) =>
        log.error("Node could not connect to bootstrap node.")
      case Left(InitializationError(msg)) =>
        log.error(msg)
        Task.delay(System.exit(-1))
      case Left(error) =>
        log.error(s"Failed! Reason: '$error")
    }
  }

  implicit private def consoleIO: ConsoleIO[Task] = {
    val console = new ConsoleReader()
    console.setHistoryEnabled(true)
    console.setPrompt("rholang $ ".green)
    console.addCompleter(new StringsCompleter(ReplRuntime.keywords.asJava))
    effects.consoleIO(console)
  }

}
