package coop.rchain.node

import cats.effect.{ExitCode, IO, IOApp}
import coop.rchain.node.configuration._
import coop.rchain.node.effects._
import coop.rchain.node.runtime.NodeMain
import coop.rchain.shared._
import org.slf4j.LoggerFactory

object Main extends IOApp {

  /**
    * Main entry point
    * @param args input args
    */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def run(args: List[String]): IO[ExitCode] = {
    // Catch-all for unhandled exceptions. Use only JDK and SLF4J.
    Thread.setDefaultUncaughtExceptionHandler((thread, ex) => {
      LoggerFactory.getLogger(getClass).error("Unhandled exception in thread " + thread.getName, ex)
    })

    implicit val console: ConsoleIO[IO] = NodeMain.consoleIO
    implicit val log: Log[IO]           = effects.log

    // Ensure terminal is restored on exit
    sys.addShutdownHook {
      console.close.unsafeRunSync()(runtime)
    }

    // Parse CLI options
    val options = commandline.Options(args)
    val x =
      if (options.subcommand.contains(options.run))
        // Start the node
        NodeMain.startNode[IO](options)
      //or
      else
        // Execute CLI command
        NodeMain.runCLI[IO](options)

    x.map(_ => ExitCode.Success)
  }
}
