from docker.client import DockerClient

from . import conftest
from .common import (
    KeyPair,
    CommandLineOptions,
)
from .rnode import (
    bootstrap_connected_peer,
    docker_network_with_started_bootstrap,
)
from .wait import (
    wait_for_node_sees_block,
    wait_for_peers_count_at_least,
    wait_for_blocks_count_at_least,
)


BOOTSTRAP_NODE_KEYPAIR = KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97')
BONDED_VALIDATOR_KEYPAIR = KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821')
JOINING_VALIDATOR_KEYPAIR = KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e')
READONLY_PEER_KEYPAIR = KeyPair(private_key='2bdedd2e4dd2e7b5f176b7a5bc155f10fafd3fbd9c03fb7556f2ffd22c786f8b', public_key='068e8311fe094e1a33646a1f8dfb50a5c12b49a1e5d0cf4cccf28d31b4a10255')


def test_heterogenous_validators(command_line_options: CommandLineOptions, docker_client: DockerClient):
    BONDED_VALIDATOR_BLOCKS = JOINING_VALIDATOR_BLOCKS = 10
    with conftest.testing_context(command_line_options, docker_client, bootstrap_keypair=BOOTSTRAP_NODE_KEYPAIR, peers_keypairs=[BONDED_VALIDATOR_KEYPAIR]) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator', keypair=BONDED_VALIDATOR_KEYPAIR) as bonded_validator:
                wait_for_peers_count_at_least(context, bonded_validator, 1)
                contract_path = '/opt/docker/examples/hello_world_again.rho'
                for _ in range(BONDED_VALIDATOR_BLOCKS):
                    bonded_validator.deploy(contract_path)
                    bonded_validator.propose()

                with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='joining-validator', keypair=JOINING_VALIDATOR_KEYPAIR) as joining_validator:
                    joining_validator.generate_faucet_bonding_deploys(
                        bond_amount=123,
                        private_key=JOINING_VALIDATOR_KEYPAIR.private_key,
                        public_key=JOINING_VALIDATOR_KEYPAIR.public_key,
                    )
                    wait_for_peers_count_at_least(context, bonded_validator, 2)
                    forward_file = joining_validator.cat_forward_file(public_key=JOINING_VALIDATOR_KEYPAIR.public_key)
                    bond_file = joining_validator.cat_bond_file(public_key=JOINING_VALIDATOR_KEYPAIR.public_key)
                    bonded_validator.deploy_string(forward_file)
                    bonded_validator.propose()
                    bonded_validator.deploy_string(bond_file)
                    bonding_block_hash = bonded_validator.propose()
                    wait_for_node_sees_block(context, joining_validator, bonding_block_hash)
                    for _ in range(JOINING_VALIDATOR_BLOCKS):
                        joining_validator.deploy(contract_path)
                        joining_validator.propose()

                    with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='readonly-peer', keypair=READONLY_PEER_KEYPAIR) as readonly_peer:
                        wait_for_peers_count_at_least(context, bonded_validator, 3)
                        # Force sync with the network
                        joining_validator.deploy(contract_path)
                        joining_validator.propose()
                        expected_blocks_count = BONDED_VALIDATOR_BLOCKS + JOINING_VALIDATOR_BLOCKS + 2
                        wait_for_blocks_count_at_least(
                            context,
                            readonly_peer,
                            expected_blocks_count,
                        )
