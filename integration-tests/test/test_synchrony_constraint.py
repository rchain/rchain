from random import Random

from docker.client import DockerClient
import pytest

from rchain.crypto import PrivateKey
from rchain.client import RClientException
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
)


BOOTSTRAP_NODE_KEYS = PrivateKey.from_hex("80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709")
BONDED_VALIDATOR_KEY_1 = PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5")
BONDED_VALIDATOR_KEY_2 = PrivateKey.from_hex("f7bfb2b3f2be909dd50beac05bece5940b1e7266816d7294291a2ff66a5d660b")
BONDED_VALIDATOR_KEY_3 = PrivateKey.from_hex("2b173084083291ac6850cb734dffb69dfcb280aeb152f0d5be979bea7827c03a")

def test_synchrony_constraint(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    """
    Create a network with 4 validators.

    Validator config situation:

    Name                stakes              synchrony_constraint_threshold
    Bootstrap           10                       0
    Validator_1         100                     0.48
    Validator_2         102                     0.4
    Validator_3         98                      0.99

    Bootstrap can propose whenever it wants.
    Validator_1 can propose only when it receives block from Validator_2 or both Bootstrap and Validator_3
    Validator_2 can propose when it receives block from Validator_1 or Validator_3
    Validator_3 can propose when it receives blocks from Bootstrap, Validator_1 and Validator_2
    """
    genesis_vault = {
        BOOTSTRAP_NODE_KEYS: 500,
        BONDED_VALIDATOR_KEY_1: 500,
        BONDED_VALIDATOR_KEY_2: 500,
        BONDED_VALIDATOR_KEY_3: 500
    }

    bonded_validator_map = {
        BOOTSTRAP_NODE_KEYS: 10,
        BONDED_VALIDATOR_KEY_1: 100,
        BONDED_VALIDATOR_KEY_2: 102,
        BONDED_VALIDATOR_KEY_3: 98
    }

    sample_contract = '/opt/docker/examples/hello_world_again.rho'

    SHARD_ID = 'test'
    with conftest.testing_context(command_line_options, random_generator, docker_client, validator_bonds_dict=bonded_validator_map, bootstrap_key=BOOTSTRAP_NODE_KEYS, wallets_dict=genesis_vault) as context, \
        ready_bootstrap_with_network(context=context, synchrony_constraint_threshold=0, shard_id=SHARD_ID) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', private_key=BONDED_VALIDATOR_KEY_1, synchrony_constraint_threshold=0.48, shard_id=SHARD_ID) as bonded_validator_1, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2',private_key=BONDED_VALIDATOR_KEY_2, synchrony_constraint_threshold=0.4, shard_id=SHARD_ID) as bonded_validator_2, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-3', private_key=BONDED_VALIDATOR_KEY_3, synchrony_constraint_threshold=0.99, shard_id=SHARD_ID) as bonded_validator_3:

        #-- bootstrap can propose twice without limits
        bootstrap_node.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        bootstrap_node.propose()
        bootstrap_node.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        b1 = bootstrap_node.propose()
        #--

        #-- validator_1 can propose when validator_2 propose block
        wait_for_node_sees_block(context, bonded_validator_1, b1)
        bonded_validator_1.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        b2 = bonded_validator_1.propose()

        wait_for_node_sees_block(context, bonded_validator_3, b2)
        bonded_validator_3.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        block_hash_2 = bonded_validator_3.propose()

        wait_for_node_sees_block(context, bonded_validator_1, block_hash_2)

        with pytest.raises(RClientException):
            bonded_validator_1.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
            bonded_validator_1.propose()

        bonded_validator_2.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        block_hash_3 = bonded_validator_2.propose()

        wait_for_node_sees_block(context, bonded_validator_1, block_hash_3)
        bonded_validator_1.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        block_hash_4 = bonded_validator_1.propose()
        #--

        #-- validator_2 can propose because validator_1 already propose
        wait_for_node_sees_block(context, bonded_validator_2, block_hash_4)
        bonded_validator_2.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        block_hash_5 = bonded_validator_2.propose()
        #--

        #-- validator_3 can propose when all other validators already propose
        wait_for_node_sees_block(context, bonded_validator_3, block_hash_5)
        with pytest.raises(RClientException):
            bonded_validator_3.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
            bonded_validator_3.propose()
        bootstrap_node.deploy(sample_contract, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
        block_hash_6 = bootstrap_node.propose()
        wait_for_node_sees_block(context, bonded_validator_3, block_hash_6)
        bonded_validator_3.propose()
        #--
