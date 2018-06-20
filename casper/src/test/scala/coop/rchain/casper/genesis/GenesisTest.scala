package coop.rchain.casper.genesis

import cats.Id
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.catscontrib._
import coop.rchain.casper.protocol.Bond
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.p2p.EffectsTestInstances.LogStub

import java.io.PrintWriter
import java.nio.file.Files

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class GenesisTest extends FlatSpec with Matchers with BeforeAndAfterEach {
  def validatorsPath = Files.createTempDirectory(s"casper-genesis-test")
  val numValidators  = 5
  implicit val log   = new LogStub[Id]

  val validators = Seq(
    "299670c52849f1aa82e8dfe5be872c16b600bf09cc8983e04b903411358f2de6",
    "6bf1b2753501d02d386789506a6d93681d2299c6edfd4455f596b97bc5725968"
  ).zipWithIndex

  def printBonds(bondsFile: String): Unit = {
    val pw = new PrintWriter(bondsFile)
    pw.println(
      validators
        .map {
          case (v, i) => s"$v $i"
        }
        .mkString("\n")
    )
    pw.close()
  }

  override def beforeEach(): Unit =
    log.reset()

  "Genesis.fromBondsFile" should "generate random validators when no bonds file is given" in {
    val _ = Genesis.fromBondsFile[Id](None, numValidators, validatorsPath)

    log.warns.isEmpty should be(true)
    log.infos.length should be(numValidators)
    log.infos.forall(_.contains("Created validator")) should be(true)
  }

  it should "generate random validators, with a warning, when bonds file does not exist" in {
    val _ = Genesis.fromBondsFile[Id](Some("not/a/real/file"), numValidators, validatorsPath)

    log.warns.length should be(1)
    log.warns.head
      .contains("does not exist. Falling back on generating random validators.") should be(true)
    log.infos.length should be(numValidators)
    log.infos.forall(_.contains("Created validator")) should be(true)
  }

  it should "generate random validators, with a warning, when bonds file cannot be parsed" in {
    val path         = validatorsPath
    val badBondsFile = path.resolve("misformatted.txt").toString

    val pw = new PrintWriter(badBondsFile)
    pw.println("xzy 1\nabc 123 7")
    pw.close()

    val _ = Genesis.fromBondsFile[Id](Some(badBondsFile), numValidators, path)

    log.warns.length should be(1)
    log.warns.head
      .contains("cannot be parsed. Falling back on generating random validators.") should be(true)
    log.infos.length should be(numValidators)
    log.infos.forall(_.contains("Created validator")) should be(true)
  }

  it should "create a genesis block with the right bonds when a proper bonds file is given" in {
    val path      = validatorsPath
    val bondsFile = path.resolve("givenBonds.txt").toString
    printBonds(bondsFile)

    val genesis = Genesis.fromBondsFile[Id](Some(bondsFile), numValidators, path)
    val bonds   = ProtoUtil.bonds(genesis)

    log.infos.isEmpty should be(true)
    validators
      .map {
        case (v, i) => Bond(ByteString.copyFrom(Base16.decode(v)), i)
      }
      .forall(
        bonds.contains(_)
      ) should be(true)
  }

  it should "detect an existing bonds file in the default location" in {
    val path      = validatorsPath
    val bondsFile = path.resolve("bonds.txt").toString
    printBonds(bondsFile)

    val genesis = Genesis.fromBondsFile[Id](None, numValidators, path)
    val bonds   = ProtoUtil.bonds(genesis)

    log.infos.length should be(1)
    validators
      .map {
        case (v, i) => Bond(ByteString.copyFrom(Base16.decode(v)), i)
      }
      .forall(
        bonds.contains(_)
      ) should be(true)
  }

}
