from random import Random

from rchain.crypto import PrivateKey
from docker.client import DockerClient

from . import conftest
from .common import CommandLineOptions
from .wait import (
    wait_for_node_sees_block,
    wait_for_peers_count_at_least,
    wait_for_block_finalized,
)
from .rnode import (
    bootstrap_connected_peer,
    ready_bootstrap_with_network,
)


BOOTSTRAP_NODE_KEY = PrivateKey.from_hex("ff2ba092524bafdbc85fa0c7eddb2b41c69bc9bf066a4711a8a16f749199e5be")
BONDED_VALIDATOR_KEY_1 = PrivateKey.from_hex("597623f0b50e82008d52644983699724538b4307efbe3d0b992c651ca7f860c8")
BONDED_VALIDATOR_KEY_2 = PrivateKey.from_hex("9a32ff7b7c6e25527e0b4e5bec70596c6094e6529d56bf61cbd1ca26d3e92b10")
BONDED_VALIDATOR_KEY_3 = PrivateKey.from_hex("af47862137d4e772f540029ae73ee01443c61288f3df9307a13d681de6ad2de4")
BONDED_VALIDATOR_KEY_4 = PrivateKey.from_hex("2a6018851984203e0983f0671e94fcf649ec04b614e5924f435081f7d1e3b44b")



def test_finalizes_block(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    """
    ^
    |
    |                               b7=-1.0
    |                                                   b6=-1.0
    |                               b5=-1.0
    |           b4=0.2631579
    |                                                   b3=0.57894737
    |                               b2=1
    |           b1=1
    |
    |           Genesis=1
    |
    |           Bootstrap           Validator1          Validator2
    | bonds         25                  20                  15
    |
    time
    """
    peers_keypairs = [BONDED_VALIDATOR_KEY_1, BONDED_VALIDATOR_KEY_2]
    contract_path = '/opt/docker/examples/tut-hello.rho'
    validator_bonds_map = {
        BOOTSTRAP_NODE_KEY:    60,
        BONDED_VALIDATOR_KEY_1: 20,
        BONDED_VALIDATOR_KEY_2: 15,
    }
    wallets_map = {
        BOOTSTRAP_NODE_KEY: 10000,
        BONDED_VALIDATOR_KEY_1: 10000,
        BONDED_VALIDATOR_KEY_2: 10000,
        BONDED_VALIDATOR_KEY_3: 10000,
        BONDED_VALIDATOR_KEY_4: 10000
    }
    FAULT_TOLERANCE_THRESHOLD = '0.5'
    SHARD_ID = 'test'
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_key=BOOTSTRAP_NODE_KEY, peers_keys=peers_keypairs, validator_bonds_dict=validator_bonds_map, wallets_dict=wallets_map) as context, \
        ready_bootstrap_with_network(context=context, cli_options={'--fault-tolerance-threshold': FAULT_TOLERANCE_THRESHOLD}, shard_id=SHARD_ID) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', private_key=BONDED_VALIDATOR_KEY_1, cli_options={'--fault-tolerance-threshold': FAULT_TOLERANCE_THRESHOLD}, shard_id=SHARD_ID) as validator1, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2', private_key=BONDED_VALIDATOR_KEY_2, cli_options={'--fault-tolerance-threshold': FAULT_TOLERANCE_THRESHOLD}, shard_id=SHARD_ID) as validator2:
            wait_for_peers_count_at_least(context, validator1, 2)
            wait_for_peers_count_at_least(context, validator2, 2)

            bootstrap_node.deploy(contract_path, BOOTSTRAP_NODE_KEY, shard_id=SHARD_ID)
            b1_hash = bootstrap_node.propose()
            wait_for_node_sees_block(context, validator1, b1_hash)
            wait_for_node_sees_block(context, validator2, b1_hash)

            validator1.deploy(contract_path, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
            b2_hash = validator1.propose()
            wait_for_node_sees_block(context, bootstrap_node, b2_hash)
            wait_for_node_sees_block(context, validator2, b2_hash)

            validator2.deploy(contract_path, BONDED_VALIDATOR_KEY_2, shard_id=SHARD_ID)
            b3_hash = validator2.propose()
            wait_for_node_sees_block(context, bootstrap_node, b3_hash)
            wait_for_node_sees_block(context, validator1, b3_hash)

            bootstrap_node.deploy(contract_path, BOOTSTRAP_NODE_KEY, shard_id=SHARD_ID)
            b4_hash = bootstrap_node.propose()
            wait_for_node_sees_block(context, validator2, b4_hash)
            wait_for_node_sees_block(context, validator1, b4_hash)

            validator1.deploy(contract_path, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
            b5_hash = validator1.propose()
            wait_for_node_sees_block(context, bootstrap_node, b5_hash)
            wait_for_node_sees_block(context, validator2, b5_hash)

            validator2.deploy(contract_path, BONDED_VALIDATOR_KEY_2, shard_id=SHARD_ID)
            b6_hash = validator2.propose()
            wait_for_node_sees_block(context, bootstrap_node, b6_hash)
            wait_for_node_sees_block(context, validator1, b6_hash)

            validator1.deploy(contract_path, BONDED_VALIDATOR_KEY_1, shard_id=SHARD_ID)
            b7_hash = validator1.propose()
            wait_for_node_sees_block(context, bootstrap_node, b7_hash)
            wait_for_node_sees_block(context, validator2, b7_hash)

            wait_for_block_finalized(context, bootstrap_node, block_hash_prefix=b3_hash)
            wait_for_block_finalized(context, validator1, block_hash_prefix=b3_hash)
            wait_for_block_finalized(context, validator2, block_hash_prefix=b3_hash)
