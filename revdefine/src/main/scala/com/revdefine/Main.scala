package com.revdefine

import java.io.File
import cats.Parallel
import cats.effect._
import cats.syntax.all._
import com.revdefine.origin.DefineRuntime
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.monix.Monixable
import coop.rchain.node.configuration.Configuration.Profile
import coop.rchain.node.configuration._
import coop.rchain.node.effects
import coop.rchain.node.web.VersionInfo
import coop.rchain.shared._
import monix.eval.Task
import monix.execution.Scheduler
import org.slf4j.LoggerFactory
import org.slf4j.bridge.SLF4JBridgeHandler

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

    // Main scheduler for all CPU bounded tasks
    // Should always be passed as implicit dependency.
    // All other schedulers should be explicit.
    implicit val scheduler: Scheduler = Scheduler.computation(
      Math.max(java.lang.Runtime.getRuntime.availableProcessors, 2),
      "node-runner",
      reporter = UncaughtExceptionLogger
    )
    implicit val log: Log[Task]           = effects.log
    implicit val eventLog: EventLog[Task] = EventLog.eventLogger

    // Parse CLI options
    val options = commandline.Options(args)
    startNode[Task](options).unsafeRunSync
  }

  /**
    * Starts RNode instance
    * @param options command line options
    */
  private def startNode[F[_]: Monixable: ConcurrentEffect: Parallel: ContextShift: Timer: Log: EventLog](
      options: commandline.Options
  )(implicit s: Scheduler): F[Unit] = Sync[F].defer {
    // Create merged configuration from CLI options and config file
    val (nodeConf, profile, configFile, kamonConf) = Configuration.build(options)
    // This system variable is used in Logger config file `node/src/main/resources/logback.xml`
    val _ = System.setProperty("rnode.data.dir", nodeConf.storage.dataDir.toString)

    // and start node
    // TODO : Enable it earlier once we have JDK with https://bugs.openjdk.java.net/browse/JDK-8218960 fixed
    // https://www.slf4j.org/legacy.html#jul-to-slf4j
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()
    for {
      _ <- Log[F].info(VersionInfo.get)
      _ <- logConfiguration[F](nodeConf, profile, configFile)
      // Create node runtime
      _ <- DefineRuntime.start[F](nodeConf, kamonConf)
    } yield ()
  }

  private def logConfiguration[F[_]: Sync: Log](
      conf: NodeConf,
      profile: Profile,
      configFile: Option[File]
  ): F[Unit] =
    Log[F].info(s"Starting with profile ${profile.name}") *>
      (if (configFile.isEmpty) Log[F].warn("No configuration file found, using defaults")
       else Log[F].info(s"Using configuration file: ${configFile.get.getAbsolutePath}")) *>
      Log[F].info(s"Running on network: ${conf.protocolServer.networkId}")

}
