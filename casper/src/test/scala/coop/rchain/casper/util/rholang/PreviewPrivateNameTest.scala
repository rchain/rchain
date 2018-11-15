package coop.rchain.casper.util.rholang

import com.google.protobuf.{ByteString, Int64Value}
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.{GPrivate}

import org.scalatest.{FlatSpec, Matchers}

class PreviewPrivateNameTest extends FlatSpec with Matchers {
  case class Scenario(pkHex: String, timestamp: Long, expectedId: String)

  val myNodePk = "464f6780d71b724525be14348b59c53dc8795346dfd7576c9f01c397ee7523e6"

  // When we deploy `new x ...` code from a javascript gRPC client,
  // we get this private name id in the log:
  // 14:06:01.453 [node-runner-43] INFO  c.r.casper.MultiParentCasperImpl - Received Deploy #1542308065454 -- new x0, x1 in {
  //   @{x1}!(...
  // [Unforgeable(0xa249b81b82572b32e9a8adc9d708be08bc85fdf19e4aca3c316e51d30b97c993), bundle+ {   Unforgeable(0x00) }]
  val jsClientFreshNode = Scenario(
    pkHex = myNodePk,
    timestamp = 1542308065454L, // 2018-11-15T18:54:25.454Z
    expectedId = "a249b81b82572b32e9a8adc9d708be08bc85fdf19e4aca3c316e51d30b97c993"
  )

  // But when testing with sbt, the actual id we get is:
  val jsClientSbt = Scenario(
    pkHex = myNodePk,
    timestamp = 1542308065454L, // 2018-11-15T18:54:25.454Z
    expectedId = "b5630d1bfb836635126ee7f2770873937933679e38146b1ddfbfcc14d7d8a787"
  )

  /* In order to test whether rand is used before the first private name
   is generated, we tried building a whole runtime and evaluating
   code, but it produces the same b5630d1bfb... id.

   import coop.rchain.casper.genesis.contracts.TestSetUtil
   import monix.execution.Scheduler.Implicits.global

   val runtime = TestSetUtil.runtime
   TestSetUtil.eval("""
   new x, out(`rho:io:stdout`) in {
   out!(["x first", *x, *out])
   }
   """, runtime)
   */

  // In case the js client was the issue, I tried `rnode deploy ...` as the client:
  // version: 0.7.1 (1977b934c9c77024539638ecd9d72e35c9b00048)
  //
  // 14:59:12.108 [node-runner-40] INFO  c.r.casper.MultiParentCasperImpl - Received Deploy #1542315551822 -- new x0, x1 in {
  //   @{x1}!(...
  // ["x first", Unforgeable(0xd8fc93442e7618dd11fd03f503a81c6076e01471311ea0cd3ce57b6b7573c7e6), bundle+ {   Unforgeable(0x00) }]
  val rnodeClientFreshNode = Scenario(
    pkHex = myNodePk,
    timestamp = 1542315551822L,
    expectedId = "d8fc93442e7618dd11fd03f503a81c6076e01471311ea0cd3ce57b6b7573c7e6"
  )

  // But again, we get a different result when testing with sbt:
  val rnodeClientSbt = Scenario(
    pkHex = myNodePk,
    timestamp = 1542315551822L,
    expectedId = "d472acf9c61e276e460de567a2b709bc9b97ff6135a812abcbaa60106d2744f9"
  )

  for (scenario <- List(jsClientSbt, rnodeClientSbt)) {
    val seed = DeployData()
      .withUser(ProtoUtil.stringToByteString(scenario.pkHex))
      .withTimestamp(scenario.timestamp)

    implicit val rand = Blake2b512Random(
      DeployData.toByteArray(seed)
    )

    ProtoUtil.stripDeployData(seed) should equal(seed)

    val id = rand.next()

    Base16.encode(id) should equal(scenario.expectedId)

    // The test should perhaps go ahead and build a GPrivate.
    // val addr = GPrivate(ByteString.copyFrom(id))
    // PrettyPrinter.buildString(addr) should equal("Unforgeable(0xa2...)")
  }
}
