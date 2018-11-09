import pytest
import conftest

from rnode_testing.rnode import start_bootstrap, create_peer
from rnode_testing.wait import wait_for

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


def test_heterogenous_validators(custom_system):
    with start_bootstrap(custom_system.docker, custom_system.config.node_startup_timeout, custom_system.config.rnode_timeout, custom_system.validators_data) as bootstrap_node:
        bonded_validator = create_peer(
            custom_system.docker,
            bootstrap_node.network,
            custom_system.validators_data.bonds_file,
            custom_system.config.rnode_timeout,
            None, # allowed_peers
            bootstrap_node,
            'bonded_validator',
            BONDED_VALIDATOR_KEYS,
        )
        try:
            contract_path = '/opt/docker/examples/hello_world_again.rho'
            for _ in range(1):
                bonded_validator.deploy(contract_path)
                bonded_validator.propose()

            joining_validator = create_peer(
                custom_system.docker,
                bootstrap_node.network,
                custom_system.validators_data.bonds_file,
                custom_system.config.rnode_timeout,
                None, # allowed_peers
                bootstrap_node,
                'joining_validator',
                JOINING_VALIDATOR_KEYS,
            )
            try:
                joining_validator.generate_faucet_bonding_deploys(
                    bond_amount=123,
                    private_key=JOINING_VALIDATOR_KEYS.private_key,
                    public_key=JOINING_VALIDATOR_KEYS.public_key,
                )
                bond_file = joining_validator.cat_bond_file(public_key=JOINING_VALIDATOR_KEYS.public_key)
                bonded_validator.deploy_string(bond_file)
                bonded_validator.propose()
                for _ in range(1):
                    joining_validator.deploy(contract_path)
                    joining_validator.propose()

                unbonded_validator = create_peer(
                    custom_system.docker,
                    bootstrap_node.network,
                    custom_system.validators_data.bonds_file,
                    custom_system.config.rnode_timeout,
                    None, # allowed_peers
                    bootstrap_node,
                    'unbonded_validator',
                    UNBONDED_VALIDATOR_KEYS,
                )
                try:
                    wait_for(lambda: unbonded_validator.get_blocks_count() == 1, 10, "Unbonded validator did not receive any blocks")
                finally:
                    unbonded_validator.cleanup()
            finally:
                joining_validator.cleanup()
        finally:
            bonded_validator.cleanup()
