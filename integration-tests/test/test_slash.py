import base64
import hashlib
import re
import sys
from random import Random
from typing import Generator, Tuple
from contextlib import contextmanager
import pytest

from docker.client import DockerClient
from rchain.crypto import PrivateKey

from . import conftest
from .common import (TestingContext,
                     CommandLineOptions)
from .node_client import (NodeClient,
                          node_protocol_client)
from .rnode import (Node,
                    bootstrap_connected_peer,
                    docker_network_with_started_bootstrap,
                    extract_validator_stake_from_bonds_validator_str)
from .wait import (wait_for_log_match,
                   wait_for_node_sees_block)


BOOTSTRAP_NODE_KEYS = PrivateKey.from_hex("ff2ba092524bafdbc85fa0c7eddb2b41c69bc9bf066a4711a8a16f749199e5be")
BONDED_VALIDATOR_KEY_1 = PrivateKey.from_hex("597623f0b50e82008d52644983699724538b4307efbe3d0b992c651ca7f860c8")
BONDED_VALIDATOR_KEY_2 = PrivateKey.from_hex("9a32ff7b7c6e25527e0b4e5bec70596c6094e6529d56bf61cbd1ca26d3e92b10")
BONDED_VALIDATOR_KEY_3 = PrivateKey.from_hex("af47862137d4e772f540029ae73ee01443c61288f3df9307a13d681de6ad2de4")


def generate_block_hash() -> bytes:
    blake = hashlib.blake2b(digest_size=32)
    blake.update(b'evil')
    return blake.digest()


def hex_block_hash(block_hash: bytes) -> bytes:
    return base64.b16encode(block_hash).lower()


@contextmanager
def three_nodes_network_with_node_client(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> Generator[Tuple[TestingContext, Node, Node, Node, NodeClient], None, None]:
    peers_keypairs = [BONDED_VALIDATOR_KEY_1, BONDED_VALIDATOR_KEY_2]
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_key=BOOTSTRAP_NODE_KEYS, peers_keys=peers_keypairs) as context, \
        docker_network_with_started_bootstrap(context=context) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', private_key=BONDED_VALIDATOR_KEY_1) as validator1, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2', private_key=BONDED_VALIDATOR_KEY_2) as validator2, \
        node_protocol_client(bootstrap_node.network, docker_client, context) as client:
        yield context, bootstrap_node, validator1, validator2, client


@pytest.mark.skipif(sys.platform in ('win32', 'cygwin', 'darwin'), reason="Only Linux docker support connection between host and container which node client needs")
def test_simple_slash(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    with three_nodes_network_with_node_client(command_line_options, random_generator, docker_client) as  (context, _ , validator1, validator2, client):
        contract = '/opt/docker/examples/tut-hello.rho'

        validator1.deploy(contract, BONDED_VALIDATOR_KEY_1)
        blockhash = validator1.propose()

        wait_for_node_sees_block(context, validator2, blockhash)

        block_info = validator1.show_block_parsed(blockhash)

        block_msg = client.block_request(block_info['blockHash'], validator1)
        evil_block_hash = generate_block_hash()

        block_msg.blockHash = evil_block_hash
        block_msg.sig = BONDED_VALIDATOR_KEY_1.sign_block_hash(evil_block_hash)

        client.send_block(block_msg, validator2)

        record_invalid = re.compile("Recording invalid block {}... for InvalidBlockHash".format(hex_block_hash(evil_block_hash)[:10].decode('utf8')))
        wait_for_log_match(context, validator2, record_invalid)

        validator2.deploy(contract, BONDED_VALIDATOR_KEY_2)

        slashed_block_hash = validator2.propose()

        block_info = validator2.show_block_parsed(slashed_block_hash)
        bonds_validators = extract_validator_stake_from_bonds_validator_str(block_info['bondsValidatorList'])

        assert bonds_validators[BONDED_VALIDATOR_KEY_1.get_public_key().to_hex()] == 0
