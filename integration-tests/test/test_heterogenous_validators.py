from random import Random

from docker.client import DockerClient
import pytest

from rchain.crypto import PrivateKey
from . import conftest
from .common import (
    CommandLineOptions,
    NonZeroExitCodeError,
)
from .rnode import (
    bootstrap_connected_peer,
    extract_validator_stake_from_bonds_validator_str,
    started_bootstrap,
    docker_network
)
from .wait import (
    wait_for_node_sees_block,
    wait_for_peers_count_at_least,
    wait_for_blocks_count_at_least,
)

from .test_wallets import get_vault_balance

BOOTSTRAP_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")
BONDED_VALIDATOR_KEY = PrivateKey.from_hex("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850")
JOINING_VALIDATOR_KEY = PrivateKey.from_hex("567ea426deaeb8233f134c3a266149fb196d6eea7d28b447dfefff92002cb400")
READONLY_PEER_KEY = PrivateKey.from_hex("3596e2e5fd14b24a6d84af04b7f0a8f13e3e68ee2ca91dc4b19550f12e61502c")

def test_heterogenous_validators(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    genesis_vault = {
        BOOTSTRAP_KEY: 500,
        BONDED_VALIDATOR_KEY: 500,
        JOINING_VALIDATOR_KEY: 500,
        READONLY_PEER_KEY: 500
    }

    bonded_validator_map = {
        BOOTSTRAP_KEY: 200,
        BONDED_VALIDATOR_KEY: 300
    }

    with conftest.testing_context(command_line_options, random_generator, docker_client, validator_bonds_dict=bonded_validator_map) as context, \
        docker_network(context, context.docker) as network, \
        conftest.temporary_wallets_file(random_generator, genesis_vault) as wallets_file, \
        started_bootstrap(context=context, network=network, wallets_file=wallets_file) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator', private_key=BONDED_VALIDATOR_KEY) as bonded_validator:
            wait_for_peers_count_at_least(context, bonded_validator, 1)

            contract_path = '/opt/docker/examples/hello_world_again.rho'
            bonded_validator.deploy(contract_path, BOOTSTRAP_KEY)
            latest_block_hash = bonded_validator.propose()
            # assure the joining validator is not bonded
            block_info = bonded_validator.show_block_parsed(latest_block_hash)
            block_validators_map = extract_validator_stake_from_bonds_validator_str(block_info['bondsValidatorList'])
            assert block_validators_map.get(JOINING_VALIDATOR_KEY.get_public_key().to_hex()) is None

            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='joining-validator', private_key=JOINING_VALIDATOR_KEY) as joining_validator:
                wait_for_peers_count_at_least(context, bonded_validator, 2)
                # new joining validator can not propose before bonding
                with pytest.raises(NonZeroExitCodeError):
                    joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEY)
                    joining_validator.propose()

                # deploy the bond contract
                bonding_block_hash = bonded_validator.deploy_contract_with_substitution(
                    substitute_dict={"%AMOUNT": "100"},
                    rho_file_path="resources/wallets/bond.rho",
                    private_key=JOINING_VALIDATOR_KEY
                )

                wait_for_node_sees_block(context, joining_validator, bonding_block_hash)

                # after bonding, the new joining validator can propose
                joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEY)
                latest_block_hash = joining_validator.propose()

                # assure the new joining validator has 100 bonded
                block_info = bonded_validator.show_block_parsed(latest_block_hash)
                block_validators_map = extract_validator_stake_from_bonds_validator_str(block_info['bondsValidatorList'])
                assert block_validators_map.get(JOINING_VALIDATOR_KEY.get_public_key().to_hex()) == 100

                vault_remain = get_vault_balance(context, joining_validator, JOINING_VALIDATOR_KEY.get_public_key().get_rev_address(), JOINING_VALIDATOR_KEY)
                assert vault_remain == 400

                # add a readonly node
                with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='readonly-peer', private_key=READONLY_PEER_KEY) as readonly_peer:
                    wait_for_peers_count_at_least(context, bonded_validator, 3)
                    # Force sync with the network
                    joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEY)
                    joining_validator.propose()
                    wait_for_blocks_count_at_least(
                        context,
                        readonly_peer,
                        5,
                    )
