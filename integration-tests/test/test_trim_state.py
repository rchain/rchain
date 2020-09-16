import os
from random import Random

from docker.client import DockerClient
from rchain.crypto import PrivateKey

from . import conftest
from .common import (
    CommandLineOptions,
)
from .rnode import (
    ready_bootstrap_with_network,
    bootstrap_connected_peer
)

from .wait import (wait_for_node_sees_block)

BOOTSTRAP_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")
BONDED_VALIDATOR_KEY_1 = PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5")
READONLY_PEER_KEY = PrivateKey.from_hex("3596e2e5fd14b24a6d84af04b7f0a8f13e3e68ee2ca91dc4b19550f12e61502c")


def test_trim_state(command_line_options: CommandLineOptions, random_generator: Random,
                    docker_client: DockerClient) -> None:
    genesis_vault = {
        BOOTSTRAP_KEY: int(5e10),
        READONLY_PEER_KEY: int(6e10)
    }

    bonded_validator_map = {
        BOOTSTRAP_KEY: 10000000,
        BONDED_VALIDATOR_KEY_1: 10000000
    }

    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_key=BOOTSTRAP_KEY,
                                  validator_bonds_dict=bonded_validator_map, wallets_dict=genesis_vault) as context, \
        ready_bootstrap_with_network(context=context, synchrony_constraint_threshold=0,
                                     cli_options={"--fault-tolerance-threshold": -1}) as bootstrap_node:
        # use fault-tolerance-threshold==-1 make sure that every block added to the bootstrap node is finalized
        relative_paths = bootstrap_node.shell_out('sh', '-c','ls /opt/docker/examples/*.rho').splitlines()

        # create some blocks to generate a new finalized state
        for _ in range(1, 10):
            # choose random contract to create randomness
            relative_path = random_generator.choice(relative_paths)
            full_path = os.path.join('/opt/docker/examples', relative_path)
            bootstrap_node.deploy(full_path, BOOTSTRAP_KEY, 100000, 1)
            latest_block_hash = bootstrap_node.propose()

        with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='trim-state-node',
                                      private_key=READONLY_PEER_KEY) as trim_state_node:
            # trim node should retrieve the latest approved block and replay
            wait_for_node_sees_block(context, trim_state_node, latest_block_hash)
            for _ in range(1, 5):
                relative_path = random_generator.choice(relative_paths)
                full_path = os.path.join('/opt/docker/examples', relative_path)
                bootstrap_node.deploy(full_path, BOOTSTRAP_KEY, 100000, 1)
                block_hash = bootstrap_node.propose()
                wait_for_node_sees_block(context, trim_state_node, block_hash)
