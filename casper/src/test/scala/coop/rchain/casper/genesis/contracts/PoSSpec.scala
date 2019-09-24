package coop.rchain.casper.genesis.contracts

import cats.implicits._
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.GenesisBuilder
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.casper.util.ConstructDeploy.{defaultSec, defaultSec2}

import scala.concurrent.duration._
import scala.io.Source

class TestMakePoSSucceeds
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_make_pos_succeeds.rho").mkString,
          defaultSec
        )
      ),
      120.seconds
    )

class TestBondingSucceeds
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_bonding_succeeds.rho").mkString,
          defaultSec
        )
      ),
      120.seconds
    )

class TestWithdrawSucceeds
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_withdraw_succeeds.rho").mkString,
          defaultSec
        )
      ),
      120.seconds
    )

class TestValidatorIsPaidAfterWithdrawal
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_validator_is_paid_after_withdraw_1.rho").mkString,
          defaultSec
        ),
        (
          Source.fromResource("PoSTest/test_validator_is_paid_after_withdraw_2.rho").mkString,
          defaultSec2
        ),
        (
          Source.fromResource("PoSTest/test_validator_is_paid_after_withdraw_3.rho").mkString,
          defaultSec
        )
      ),
      120.seconds
    )

class TestBondingFailsIfDepositFails
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_bonding_fails_if_deposit_fails.rho").mkString,
          defaultSec
        )
      ),
      120.seconds
    )

class TestMultipleBondingSucceeds
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_bonding_succeeds.rho").mkString,
          defaultSec
        ),
        (
          Source.fromResource("PoSTest/test_bonding_succeeds.rho").mkString,
          defaultSec2
        )
      ),
      120.seconds
    )

class PoSSpec
    extends RhoSpec(
      Seq((Source.fromResource("PoSTest.rho").mkString, defaultSec)),
      120.seconds,
      genesisParameters = GenesisBuilder
        .buildGenesisParameters()
        .map(genesis => genesis.copy(vaults = genesis.vaults ++ PoSSpec.testVaults))
    )

object PoSSpec {

  def prepareVault(vaultData: (String, Long)): Vault =
    Vault(RevAddress.fromPublicKey(PublicKey(Base16.decode(vaultData._1).get)).get, vaultData._2)

  val testVaults: Seq[Vault] = Seq(
    (
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      10000L
    ),
    (
      "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
      10000L
    ),
    (
      "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      10000L
    ),
    (
      "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
      10000L
    ),
    (
      "2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
      10000L
    ),
    (
      "3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333",
      10000L
    ),
    (
      "4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444",
      10000L
    ),
    (
      "5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555",
      10000L
    ),
    (
      "6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666",
      10000L
    ),
    (
      "7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777",
      10000L
    ),
    (
      "8888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888",
      10000L
    ),
    (
      "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
      10000L
    ),
    (
      "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
      10000L
    ),
    (
      "047b43d6548b72813b89ac1b9f9ca67624a8b372feedd71d4e2da036384a3e1236812227e524e6f237cde5f80dbb921cac12e6500791e9a9ed1254a745a816fe1f",
      10000L
    )
  ).map(prepareVault)
}
