package coop.rchain.node.configuration.commandline

import java.nio.file.Path

import collection.JavaConverters._
import collection.mutable
import scala.concurrent.duration.{FiniteDuration, _}

import coop.rchain.catscontrib.ski._
import coop.rchain.comm.PeerNode
import coop.rchain.node.configuration.hocon._

import com.typesafe.config._
import org.rogach.scallop.ScallopOption

object ConfigMapper {

  def fromOptions(options: Options): Config = {
    val map      = mutable.Map[String, Any]()
    val addToMap = AddScallopOptionToMap(map)

    {
      import GrpcServer._
      val add = addToMap(Key)
      add(keys.Host, options.grpcHost)
      add(keys.PortExternal, options.grpcPort)
      add(keys.PortInternal, options.grpcPortInternal)
      add(keys.MaxMessageSize, options.grpcMaxMessageSize)
    }

    if (options.subcommand.contains(options.run)) {
      val run = options.run

      {
        import Server._
        val add = addToMap(Key)
        add(keys.NetworkId, run.network)
        add(keys.Host, run.host)
        add(keys.HostDynamic, run.dynamicHostAddress)
        add(keys.Upnp, run.noUpnp.map(kp(false)))
        add(keys.Port, run.port)
        add(keys.PortHttp, run.httpPort)
        add(keys.PortKademlia, run.kademliaPort)
        add(keys.UseRandomPorts, run.useRandomPorts)
        add(keys.SendTimeout, run.defaultTimeout.map(_.millis))
        add(keys.Standalone, run.standalone)
        add(keys.Bootstrap, run.bootstrap)
        add(keys.DataDir, run.dataDir)
        add(keys.StoreSize, run.casperBlockStoreSize)
        add(keys.DagStorageSize, run.casperBlockDagStorageSize)
        add(keys.MapSize, run.mapSize)
        add(keys.MaxConnections, run.maxNumOfConnections)
        add(keys.AllowPrivateAddresses, run.allowPrivateAddresses)
        add(keys.MaxMessageSize, run.maxMessageSize)
        add(keys.MaxStreamMessageSize, run.maxStreamMessageSize)
        add(keys.PacketChunkSize, run.packetChunkSize)
        add(keys.MessageConsumers, run.messageConsumers)
        add(keys.FaultToleranceThreshold, run.faultToleranceThreshold)
        add(keys.SynchronyConstraintThreshold, run.synchronyConstraintThreshold)
        add(keys.HeightConstraintThreshold, run.heightConstraintThreshold)
        add(keys.Reporting, run.reporting)
      }

      {
        import RoundRobinDispatcher._
        val add = addToMap(Key)
        add(keys.MaxPeerQueueSize, run.maxPeerQueueSize)
        add(keys.GiveUpAfterSkipped, run.giveUpAfterSkipped)
        add(keys.DropPeerAfterRetries, run.dropPeerAfterRetries)
      }

      {
        import Tls._
        val add = addToMap(Key)
        add(keys.Certificate, run.certificate)
        add(keys.Key, run.key)
        add(keys.SecureRandomNonBlocking, run.secureRandomNonBlocking)
      }

      {
        import Kamon._
        val add = addToMap(Key)
        add(keys.Prometheus, run.prometheus)
        add(keys.Influxdb, run.influxdb)
        add(keys.InfluxdbUdp, run.influxdbUdp)
        add(keys.Zipkin, run.zipkin)
        add(keys.Sigar, run.sigar)
      }

      {
        import Casper._
        val add = addToMap(Key)
        add(keys.ValidatorPublicKey, run.validatorPublicKey)
        add(keys.ValidatorPrivateKey, run.validatorPrivateKey)
        add(keys.ValidatorPrivateKeyPath, run.validatorPrivateKeyPath)
        add(keys.BondsFile, run.bondsFile)
        add(keys.KnownValidatorsFile, run.knownValidators)
        add(keys.Validators, run.numValidators)
        add(keys.WalletsFile, run.walletsFile)
        add(keys.BondMinimum, run.minimumBond)
        add(keys.BondMaximum, run.maximumBond)
        add(keys.EpochLength, run.epochLength)
        add(keys.QuarantineLength, run.quarantineLength)
        add(keys.NumberOfActiveValidators, run.numberOfActiveValidators)
        add(keys.CasperLoopInterval, run.casperLoopInterval)
        add(keys.RequestedBlocksTimeout, run.requestedBlocksTimeout)
        add(keys.RequiredSignatures, run.requiredSigs)
        add(keys.Shard, run.shardId)
        add(keys.GenesisValidator, run.genesisValidator)
        add(keys.GenesisApproveInterval, run.interval)
        add(keys.GenesisApproveDuration, run.duration)
        add(keys.DeployTimestamp, run.deployTimestamp)
        add(keys.FinalizationRate, run.finalizationRate)
        add(keys.MaxNumberOfParents, run.maxNumberOfParents)
        add(keys.MaxParentDepth, run.maxParentDepth)
      }
    }

    ConfigFactory.parseMap(map.asJava, "command line options")
  }

  private trait OptionConverter[A] {
    def toConfigValue(a: A): Any
  }

  private object OptionConverter {
    def apply[A](implicit C: OptionConverter[A]): OptionConverter[A] = C

    implicit val stringConverter: OptionConverter[String]     = (a: String) => a
    implicit val booleanConverter: OptionConverter[Boolean]   = (b: Boolean) => b
    implicit val flagConverter: OptionConverter[Options.Flag] = (f: Options.Flag) => f
    implicit val intConverter: OptionConverter[Int]           = (i: Int) => i
    implicit val longConverter: OptionConverter[Long]         = (l: Long) => l
    implicit val floatConverter: OptionConverter[Float]       = (f: Float) => f
    implicit val pathConverter: OptionConverter[Path]         = (p: Path) => p.toString
    implicit val peerNodeConverter: OptionConverter[PeerNode] = (p: PeerNode) => p.toAddress
    implicit val durationConverter: OptionConverter[FiniteDuration] =
      (d: FiniteDuration) => java.time.Duration.ofNanos(d.toNanos)
  }

  private object AddScallopOptionToMap {
    def apply(map: mutable.Map[String, Any]): AddToMap = new AddToMap(map)

    final class AddToMap(map: mutable.Map[String, Any]) {
      def apply(prefix: String): AddWithPrefix = new AddWithPrefix(prefix)

      @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
      final class AddWithPrefix(prefix: String) {
        def apply[A: OptionConverter](key: String, opt: ScallopOption[A]): Unit =
          opt.foreach(a => map += s"$prefix.$key" -> OptionConverter[A].toConfigValue(a))
      }
    }
  }
}
