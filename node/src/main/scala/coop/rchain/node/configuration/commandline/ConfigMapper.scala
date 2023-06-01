package coop.rchain.node.configuration.commandline

import com.typesafe.config._
import coop.rchain.comm.PeerNode
import org.rogach.scallop.ScallopOption

import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

object ConfigMapper {

  def fromOptions(options: Options): Config = {
    val map      = mutable.Map[String, Any]()
    val addToMap = AddScallopOptionToMap(map)

    if (options.subcommand.contains(options.run)) {
      val run = options.run
      val add = addToMap()
      add("standalone", run.standalone)
      add("autopropose", run.autopropose)
      add("protocol-server.network-id", run.networkId)
      add("protocol-server.dynamic-ip", run.dynamicIp)
      add("protocol-server.no-upnp", run.noUpnp)
      add("protocol-server.host", run.host)
      add("protocol-server.port", run.protocolPort)
      add("protocol-server.use-random-ports", run.useRandomPorts)
      add(
        "protocol-server.disable-state-exporter",
        run.disableStateExporter
      )
      add(
        "protocol-server.grpc-max-recv-message-size",
        run.protocolGrpcMaxRecvMessageSize
      )
      add(
        "protocol-server.grpc-max-recv-stream-message-size",
        run.protocolGrpcMaxRecvStreamMessageSize
      )
      add("protocol-server.max-message-consumers", run.protocolMaxMessageConsumers)

      add("peers-discovery.port", run.discoveryPort)
      add("peers-discovery.lookup-interval", run.discoveryLookupInterval)
      add("peers-discovery.cleanup-interval", run.discoveryCleanupInterval)
      add("peers-discovery.heartbeat-batch-size", run.discoveryHeartbeatBatchSize)
      add("peers-discovery.init-wait-loop-interval", run.discoveryInitWaitLoopInterval)

      add("protocol-client.bootstrap", run.bootstrap)
      add("protocol-client.network-timeout", run.networkTimeout)
      add("protocol-client.batch-max-connections", run.protocolMaxConnections)
      add(
        "protocol-client.grpc-max-recv-message-size",
        run.protocolGrpcMaxRecvMessageSize
      )
      add("protocol-client.grpc-stream-chunk-size", run.protocolGrpcStreamChunkSize)
      add("protocol-client.disable-lfs", run.disableLfs)

      add("storage.data-dir", run.dataDir)

      add("casper.shard-name", run.shardName)
      add("casper.max-number-of-parents", run.maxNumberOfParents)
      add("casper.synchrony-constraint-threshold", run.synchronyConstraintThreshold)
      add("casper.height-constraint-threshold", run.heightConstraintThreshold)
      add("casper.validator-public-key", run.validatorPublicKey)
      add("casper.validator-private-key", run.validatorPrivateKey)
      add("casper.validator-private-key-path", run.validatorPrivateKeyPath)
      add("casper.casper-loop-interval", run.casperLoopInterval)
      add("casper.requested-blocks-timeout", run.requestedBlocksTimeout)
      add("casper.fork-choice-stale-threshold", run.forkChoiceStaleThreshold)
      add("casper.fork-choice-check-if-stale-interval", run.forkChoiceCheckIfStaleInterval)

      add("casper.genesis-block-data.bonds-file", run.bondsFile)
      add("casper.genesis-block-data.wallets-file", run.walletsFile)
      add("casper.genesis-block-data.bond-minimum", run.bondMinimum)
      add("casper.genesis-block-data.bond-maximum", run.bondMaximum)
      add("casper.genesis-block-data.epoch-length", run.epochLength)
      add("casper.genesis-block-data.quarantine-length", run.quarantineLength)
      add("casper.genesis-block-data.number-of-active-validators", run.numberOfActiveValidators)
      add("casper.genesis-block-data.pos-vault-pub-key", run.posVaultPubKey)
      add("casper.genesis-block-data.system-contract-pub-key", run.systemContractPubKey)
      add("casper.genesis-block-data.genesis-block-number", run.genesisBlockNumber)
      add("casper.genesis-block-data.pos-multi-sig-public-keys", run.posMultiSigPublicKeys)
      add("casper.genesis-block-data.pos-multi-sig-quorum", run.posMultiSigQuorum)

      add("casper.autogen-shard-size", run.autogenShardSize)

      add("casper.min-phlo-price", run.minPhloPrice)

      add("api-server.port-grpc-external", run.apiPortGrpcExternal)
      add("api-server.port-grpc-internal", run.apiPortGrpcInternal)
      add("api-server.grpc-max-recv-message-size", run.apiGrpcMaxRecvMessageSize)
      add("api-server.host", run.apiHost)
      add("api-server.port-http", run.apiPortHttp)
      add("api-server.port-admin-http", run.apiPortAdminHttp)
      add("api-server.enable-reporting", run.apiEnableReporting)
      add("api-server.max-blocks-limit", run.apiMaxBlocksLimit)
      add("api-server.keep-alive-time", run.apiKeepAliveTime)
      add("api-server.keep-alive-timeout", run.apiKeepAliveTimeout)
      add("api-server.permit-keep-alive-time", run.apiPermitKeepAliveTime)
      add("api-server.max-connection-idle", run.apiMaxConnectionIdle)
      add("api-server.max-connection-age", run.apiMaxConnectionAge)
      add("api-server.max-connection-age-grace", run.apiMaxConnectionAgeGrace)

      add("tls.key-path", run.tlsKeyPath)
      add("tls.certificate-path", run.tlsCertificatePath)
      add("tls.secure-random-non-blocking", run.tlsSecureRandomNonBlocking)

      add("metrics.prometheus", run.prometheus)
      add("metrics.influxdb", run.influxdb)
      add("metrics.influxdb-udp", run.influxdbUdp)
      add("metrics.zipkin", run.zipkin)
      add("metrics.sigar", run.sigar)

      add("dev-mode", run.devMode)
      add("dev.deployer-private-key", run.deployerPrivateKey)
    }
    ConfigFactory.parseMap(map.asJava)
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
    implicit val listOfStringConverter: OptionConverter[List[String]] = (l: List[String]) =>
      l.mkString(" ")
  }

  private object AddScallopOptionToMap {
    def apply(map: mutable.Map[String, Any]): AddToMap = new AddToMap(map)

    final class AddToMap(map: mutable.Map[String, Any]) {
      def apply(): AddWithPrefix = new AddWithPrefix()

      @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
      final class AddWithPrefix() {
        def apply[A: OptionConverter](key: String, opt: ScallopOption[A]): Unit =
          opt.foreach(a => map += s"$key" -> OptionConverter[A].toConfigValue(a))
      }
    }
  }
}
