import pytest
import conftest
import contextlib

from rnode_testing.rnode import start_bootstrap, create_peer
from rnode_testing.wait import wait_for, node_started
from rnode_testing.network import (
    wait_for_approved_block_received_handler_state,
)


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


@pytest.yield_fixture(scope='session')
def validators_config():
    validator_keys = [BONDED_VALIDATOR_KEYS]
    with conftest.temporary_bonds_file(validator_keys) as f:
        yield conftest.ValidatorsData(bonds_file=f, bootstrap_keys=BOOTSTRAP_NODE_KEYS, peers_keys=validator_keys)


@pytest.yield_fixture(scope="session")
def custom_system(request, validators_config, docker_client_session):
    test_config = conftest.make_test_config(request)
    yield conftest.System(test_config, docker_client_session, validators_config)


@contextlib.contextmanager
def started_bonded_validator(system, bootstrap_node):
    bonded_validator = create_peer(
        docker_client=system.docker,
        network=bootstrap_node.network,
        name='bonded-validator',
        bonds_file=system.validators_data.bonds_file,
        rnode_timeout=system.config.rnode_timeout,
        bootstrap=bootstrap_node,
        key_pair=BONDED_VALIDATOR_KEYS,
    )
    try:
        wait_for(node_started(bonded_validator), system.config.node_startup_timeout, "Bonded validator node didn't start correctly")
        wait_for_approved_block_received_handler_state(bonded_validator, system.config.node_startup_timeout)
        yield bonded_validator
    finally:
        bonded_validator.cleanup()


@contextlib.contextmanager
def started_joining_validator(system, bootstrap_node):
    joining_validator = create_peer(
        docker_client=system.docker,
        network=bootstrap_node.network,
        name='joining-validator',
        bonds_file=system.validators_data.bonds_file,
        rnode_timeout=system.config.rnode_timeout,
        bootstrap=bootstrap_node,
        key_pair=JOINING_VALIDATOR_KEYS,
    )
    try:
        wait_for(node_started(joining_validator), system.config.node_startup_timeout, "Joining validator node didn't start correctly")
        wait_for_approved_block_received_handler_state(joining_validator, system.config.node_startup_timeout)
        yield joining_validator
    finally:
        joining_validator.cleanup()



@contextlib.contextmanager
def started_unbonded_validator(system, bootstrap_node,name='unbonded-validator'):
    unbonded_validator = create_peer(
        docker_client=system.docker,
        network=bootstrap_node.network,
        name=name,
        bonds_file=system.validators_data.bonds_file,
        rnode_timeout=system.config.rnode_timeout,
        bootstrap=bootstrap_node,
        key_pair=UNBONDED_VALIDATOR_KEYS,
    )
    try:
        wait_for(node_started(unbonded_validator), system.config.node_startup_timeout, "Unbonded validator node didn't start correctly")
        wait_for_approved_block_received_handler_state(unbonded_validator, system.config.node_startup_timeout)
        yield unbonded_validator
    finally:
        unbonded_validator.cleanup()



def test_heterogenous_validators(custom_system):
    BONDED_VALIDATOR_BLOCKS = 10
    JOINING_VALIDATOR_BLOCKS = 10
    with start_bootstrap(custom_system.docker, custom_system.config.node_startup_timeout, custom_system.config.rnode_timeout, custom_system.validators_data) as bootstrap_node:
        with started_bonded_validator(custom_system, bootstrap_node) as bonded_validator:
            contract_path = '/opt/docker/examples/hello_world_again.rho'
            for _ in range(BONDED_VALIDATOR_BLOCKS):
                bonded_validator.deploy(contract_path)
                bonded_validator.propose()

            with started_joining_validator(custom_system, bootstrap_node) as joining_validator:
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

                with started_unbonded_validator(custom_system, bootstrap_node) as unbonded_validator:
                    # Force sync with the network
                    joining_validator.deploy(contract_path)
                    joining_validator.propose()
                    def condition():
                        expected_blocks_count = BONDED_VALIDATOR_BLOCKS + JOINING_VALIDATOR_BLOCKS
                        actual_blocks_count = unbonded_validator.get_blocks_count(30)
                        if actual_blocks_count < expected_blocks_count:
                            raise Exception("Expected {} blocks, got {}".format(expected_blocks_count, actual_blocks_count))
                    wait_for(condition, 600, "Unbonded validator did not receive any blocks")


def     (custom_system):
    BONDED_VALIDATOR_BLOCKS = 5
    JOINING_VALIDATOR_BLOCKS = 3
    with start_bootstrap(custom_system.docker, custom_system.config.node_startup_timeout, custom_system.config.rnode_timeout, custom_system.validators_data) as bootstrap_node:
        with started_bonded_validator(custom_system, bootstrap_node) as bonded_validator:
            contract_path = '/opt/docker/examples/hello_world_again.rho'
            for _ in range(BONDED_VALIDATOR_BLOCKS):
                bonded_validator.deploy(contract_path)
                bonded_validator.propose()

            with started_unbonded_validator(custom_system, bootstrap_node, 'ronode') as ronode:
                for _ in range(BONDED_VALIDATOR_BLOCKS):
                    bonded_validator.deploy(contract_path)
                    bonded_validator.propose()

                def condition_ronode():
                    expected_blocks_count = 2 * BONDED_VALIDATOR_BLOCKS
                    actual_blocks_count = ronode.get_blocks_count()
                    if actual_blocks_count < expected_blocks_count:
                        raise Exception("Expected {} blocks, got {}".format(expected_blocks_count, actual_blocks_count))
                
                wait_for(condition_ronode, 600, "Unbonded validator did not receive any blocks")

                with started_joining_validator(custom_system, bootstrap_node) as joining_validator:
                    joining_validator.generate_faucet_bonding_deploys(
                        bond_amount=123,
                        private_key=JOINING_VALIDATOR_KEYS.private_key,
                        public_key=JOINING_VALIDATOR_KEYS.public_key,
                    )
                    bond_file = joining_validator.cat_bond_file(public_key=JOINING_VALIDATOR_KEYS.public_key)
                    bonded_validator.deploy_string(bond_file)
                    bonded_validator.propose()
                    
                    for _ in range(JOINING_VALIDATOR_BLOCKS):
                        joining_validator.deploy(contract_path)
                        joining_validator.propose()

                    with started_unbonded_validator(custom_system, bootstrap_node) as unbonded_validator:
                        for _ in range(JOINING_VALIDATOR_BLOCKS):
                            joining_validator.deploy(contract_path)
                            joining_validator.propose()
                        
                        def condition_unbonded():
                            expected_blocks_count = 2 * BONDED_VALIDATOR_BLOCKS + 2 * JOINING_VALIDATOR_BLOCKS + 1
                            actual_blocks_count = unbonded_validator.get_blocks_count()
                            if actual_blocks_count < expected_blocks_count:
                                raise Exception("Expected {} blocks, got {}".format(expected_blocks_count, actual_blocks_count))
                        wait_for(condition_unbonded, 600, "Unbonded validator did not receive any blocks")

                        def condition_ronode2():
                            expected_blocks_count = 2 * BONDED_VALIDATOR_BLOCKS + 2 * JOINING_VALIDATOR_BLOCKS + 1
                            actual_blocks_count = ronode.get_blocks_count()
                            if actual_blocks_count < expected_blocks_count:
                                raise Exception("Expected {} blocks, got {}".format(expected_blocks_count, actual_blocks_count))
                        wait_for(condition_ronode2, 600, "RO Node did not receive any blocks")
