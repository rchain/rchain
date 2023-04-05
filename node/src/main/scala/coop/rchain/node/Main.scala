package coop.rchain.node

import cats.effect.{ContextShift, IO, Timer}
import coop.rchain.node.configuration._
import coop.rchain.node.effects._
import coop.rchain.node.runtime.NodeMain
import coop.rchain.shared._
import monix.execution.Scheduler
import org.slf4j.LoggerFactory

object Main {

  /**
    * Main entry point
    * @param args input args
    */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    // Catch-all for unhandled exceptions. Use only JDK and SLF4J.
    Thread.setDefaultUncaughtExceptionHandler((thread, ex) => {
      LoggerFactory.getLogger(getClass).error("Unhandled exception in thread " + thread.getName, ex)
    })

    // Main scheduler for all CPU bounded tasks and ContextShift
    import RChainScheduler._

    implicit val console: ConsoleIO[IO] = NodeMain.consoleIO
    implicit val log: Log[IO]           = effects.log

    // Ensure terminal is restored on exit
    sys.addShutdownHook {
      console.close.unsafeRunSync
    }

    // Parse CLI options
    val options = commandline.Options(args)
    if (options.subcommand.contains(options.run))
      // Start the node
      NodeMain.startNode[IO](options).unsafeRunSync
    //or
    else
      // Execute CLI command
      NodeMain.runCLI[IO](options).unsafeRunSync
  }
}
