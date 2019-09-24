package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy.{defaultPub, defaultSec, defaultSec2}
import coop.rchain.casper.util.GenesisBuilder
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1

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

class TestPaySucceeds
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_pay_succeeds_1.rho").mkString,
          defaultSec
        ),
        (
          PoSSpec.templateSource(
            Source.fromResource("PoSTest/test_pay_succeeds_2.rho").mkString,
            Map(
              "validatorPk" -> Base16
                .encode(Secp256k1.toPublic(GenesisBuilder.defaultValidatorSks(1)).bytes)
            )
          ),
          defaultSec2
        )
      ),
      120.seconds
    )

class TestBondingFailsIfAlreadyBonded
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_bonding_fails_if_already_bonded.rho").mkString,
          defaultSec
        )
      ),
      120.seconds
    )

class TestBondingFailsIfBondTooSmall
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_bonding_fails_if_bond_too_small.rho").mkString,
          defaultSec
        )
      ),
      120.seconds
    )

class TestDontPayInactiveValidators
    extends RhoSpec(
      Seq(
        (
          Source.fromResource("PoSTest/test_dont_pay_inactive_validators_1.rho").mkString,
          defaultSec
        ),
        (
          PoSSpec.templateSource(
            Source.fromResource("PoSTest/test_dont_pay_inactive_validators_2.rho").mkString,
            Map("validatorPk" -> Base16.encode(defaultPub.bytes))
          ),
          defaultSec2
        )
      ),
      120.seconds
    )

object PoSSpec {
  def templateSource(source: String, substitutions: Map[String, String]): String =
    substitutions.foldLeft(source) {
      case (acc, (name, value)) => acc.replace(s"""$$$$$name$$$$""", value.toString)
    }
}
