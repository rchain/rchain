from random import Random

import pytest

from rchain.crypto import PrivateKey
from rchain.client import RClientException
from docker.client import DockerClient
from . import conftest
from .common import (
    CommandLineOptions,
)
from .rnode import (
    bootstrap_connected_peer,
    ready_bootstrap_with_network,
)

from .wait import (
    wait_for_node_sees_block,
    wait_for_peers_count_at_least,
    wait_for_approved_block_received_handler_state
)

from .test_wallets import get_vault_balance

BOOTSTRAP_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")
BONDED_VALIDATOR_KEY = PrivateKey.from_hex("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850")
JOINING_VALIDATOR_KEY = PrivateKey.from_hex("567ea426deaeb8233f134c3a266149fb196d6eea7d28b447dfefff92002cb400")
READONLY_PEER_KEY = PrivateKey.from_hex("3596e2e5fd14b24a6d84af04b7f0a8f13e3e68ee2ca91dc4b19550f12e61502c")

def test_bonding_validators(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    genesis_vault = {
        BOOTSTRAP_KEY: 50000000,
        BONDED_VALIDATOR_KEY: 50000000,
        JOINING_VALIDATOR_KEY: 50000000,
        READONLY_PEER_KEY: 50000000
    }

    bonded_validator_map = {
        BOOTSTRAP_KEY: 10000000,
        BONDED_VALIDATOR_KEY: 10000000
    }

    epoch_length = 4
    quarantine_length = 20
    with conftest.testing_context(command_line_options, random_generator, docker_client, validator_bonds_dict=bonded_validator_map, wallets_dict=genesis_vault) as context, \
        ready_bootstrap_with_network(context=context, synchrony_constraint_threshold=0, epoch_length=epoch_length, quarantine_length=quarantine_length) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator', private_key=BONDED_VALIDATOR_KEY, epoch_length=epoch_length, quarantine_length=quarantine_length) as bonded_validator:
            wait_for_peers_count_at_least(context, bonded_validator, 1)
            # genesis block number is 0

            contract_path = '/opt/docker/examples/hello_world_again.rho'

            # block number 1
            bonded_validator.deploy(contract_path, BOOTSTRAP_KEY)
            latest_block_hash = bonded_validator.propose()

            # assure the joining validator is not bonded
            block_info = bonded_validator.get_block(latest_block_hash)
            assert JOINING_VALIDATOR_KEY.get_public_key().to_hex() not in [b.validator for b in block_info.blockInfo.bonds]

            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='joining-validator', private_key=JOINING_VALIDATOR_KEY) as joining_validator:
                wait_for_peers_count_at_least(context, bonded_validator, 2)
                wait_for_approved_block_received_handler_state(context, joining_validator)

                # new joining validator can not propose before bonding
                with pytest.raises(RClientException):
                    joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEY)
                    joining_validator.propose()

                # block number 2
                # deploy the bond contract
                # next epoch change is block number 4
                bond_amount = 10000000
                bonding_block_hash = bonded_validator.deploy_contract_with_substitution(
                    substitute_dict={"%AMOUNT": "{}".format(bond_amount)},
                    rho_file_path="resources/wallets/bond.rho",
                    private_key=JOINING_VALIDATOR_KEY,
                    phlo_limit=1000000,
                    phlo_price=1
                )

                # bonding map already has the bond message
                # but the validator is not active
                block_info = bonded_validator.get_block(bonding_block_hash)
                bonds = {b.validator: b.stake for b in block_info.blockInfo.bonds}
                assert bonds.get(JOINING_VALIDATOR_KEY.get_public_key().to_hex()) == bond_amount

                # block number 3
                bonded_validator.deploy(contract_path, BOOTSTRAP_KEY)
                b3 = bonded_validator.propose()

                wait_for_node_sees_block(context, joining_validator, b3)

                # block number 4 is not proposed yet
                # joining validator is still not active
                with pytest.raises(RClientException):
                    joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEY)
                    joining_validator.propose()

                # block number 4
                bonded_validator.deploy(contract_path, BOOTSTRAP_KEY)
                b4 = bonded_validator.propose()

                wait_for_node_sees_block(context, joining_validator, b4)


                # after block number 4
                # after bonding, the new joining validator can propose
                # block number 5
                joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEY)
                b5 = joining_validator.propose()

                wait_for_node_sees_block(context, bonded_validator, b5)

                vault_remain = get_vault_balance(context, joining_validator, JOINING_VALIDATOR_KEY.get_public_key().get_rev_address(), JOINING_VALIDATOR_KEY, 100000, 1)
                assert vault_remain < 50000000 - bond_amount
