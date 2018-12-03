import contextlib

import pytest
import conftest

from rnode_testing.common import TestingContext
from rnode_testing.rnode import (
    docker_network_with_started_bootstrap,
    started_peer,
    Node,
)
from rnode_testing.wait import (
    wait_for_blocks_count_at_least,
    wait_for_approved_block_received_handler_state,
)

from typing import Generator, TYPE_CHECKING

if TYPE_CHECKING:
    from _pytest.fixtures import SubRequest
    from docker.client import DockerClient
    from rnode_testing.rnode import Node

"""
First approximation:
1) Bootstrap node with a bonds file in the genesis block for at least one bonded validator
2) Bonded validator joins the network and proposes to create a block chain length 10
3) Unbounded validator joins and goes through the process to bond.
4) Validators create a blockchain length 20
5) Unbounded validator joins and attempts to catch the state (20 blocks)
"""


"""
Second approximation:
1) Create boostrap node
2) Create validator B bonded with the bootstrap node
3) B executes propose 10 times in a row
4) Create new validator U
5) Make U bonded
6) Execute 10 propose operations
7) Create new validator N and wait until it catches up
"""


BOOTSTRAP_NODE_KEYS = conftest.KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97')
BONDED_VALIDATOR_KEYS = conftest.KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821')
JOINING_VALIDATOR_KEYS = conftest.KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e')
UNBONDED_VALIDATOR_KEYS = conftest.KeyPair(private_key='2bdedd2e4dd2e7b5f176b7a5bc155f10fafd3fbd9c03fb7556f2ffd22c786f8b', public_key='068e8311fe094e1a33646a1f8dfb50a5c12b49a1e5d0cf4cccf28d31b4a10255')


@contextlib.contextmanager
def started_bonded_validator(context: TestingContext, bootstrap_node: Node):
    with started_peer(
        context=context,
        network=bootstrap_node.network,
        name='bonded-validator',
        bootstrap=bootstrap_node,
        key_pair=BONDED_VALIDATOR_KEYS,
    ) as bonded_validator:
        wait_for_approved_block_received_handler_state(bonded_validator, context.node_startup_timeout)
        yield bonded_validator


@contextlib.contextmanager
def started_joining_validator(context: TestingContext, bootstrap_node: Node):
    with started_peer(
        context=context,
        network=bootstrap_node.network,
        name='joining-validator',
        bootstrap=bootstrap_node,
        key_pair=JOINING_VALIDATOR_KEYS,
    ) as joining_validator:
        wait_for_approved_block_received_handler_state(joining_validator, context.node_startup_timeout)
        yield joining_validator



@contextlib.contextmanager
def started_readonly_peer(context: TestingContext, bootstrap_node: Node):
    with started_peer(
        context=context,
        network=bootstrap_node.network,
        name='unbonded-validator',
        bootstrap=bootstrap_node,
        key_pair=UNBONDED_VALIDATOR_KEYS,
    ) as readonly_peer:
        wait_for_approved_block_received_handler_state(readonly_peer, context.node_startup_timeout)
        yield readonly_peer



@pytest.mark.xfail
def test_heterogenous_validators(command_line_options_fixture, docker_client_fixture):
    BONDED_VALIDATOR_BLOCKS = 10
    JOINING_VALIDATOR_BLOCKS = 10
    with conftest.testing_context(command_line_options_fixture, docker_client_fixture, bootstrap_keypair=BOOTSTRAP_NODE_KEYS, peers_keypairs=[BONDED_VALIDATOR_KEYS]) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            with started_bonded_validator(context, bootstrap_node) as bonded_validator:
                contract_path = '/opt/docker/examples/hello_world_again.rho'
                for _ in range(BONDED_VALIDATOR_BLOCKS):
                    bonded_validator.deploy(contract_path)
                    bonded_validator.propose()

                with started_joining_validator(context, bootstrap_node) as joining_validator:
                    joining_validator.generate_faucet_bonding_deploys(
                        bond_amount=123,
                        private_key=JOINING_VALIDATOR_KEYS.private_key,
                        public_key=JOINING_VALIDATOR_KEYS.public_key,
                    )
                    forward_file = joining_validator.cat_forward_file(public_key=JOINING_VALIDATOR_KEYS.public_key)
                    bond_file = joining_validator.cat_bond_file(public_key=JOINING_VALIDATOR_KEYS.public_key)
                    bonded_validator.deploy_string(forward_file)
                    bonded_validator.propose()
                    bonded_validator.deploy_string(bond_file)
                    bonded_validator.propose()
                    for _ in range(JOINING_VALIDATOR_BLOCKS):
                        joining_validator.deploy(contract_path)
                        joining_validator.propose()

                    with started_readonly_peer(context, bootstrap_node) as readonly_peer:
                        # Force sync with the network
                        joining_validator.deploy(contract_path)
                        joining_validator.propose()
                        expected_blocks_count = BONDED_VALIDATOR_BLOCKS + JOINING_VALIDATOR_BLOCKS
                        max_retrieved_blocks = 30
                        wait_for_blocks_count_at_least(
                            readonly_peer,
                            expected_blocks_count,
                            max_retrieved_blocks,
                            expected_blocks_count * 10,
                        )
