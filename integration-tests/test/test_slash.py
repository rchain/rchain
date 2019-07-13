import base64
import hashlib
import re
import sys
from random import Random

import pytest

from docker.client import DockerClient

from . import conftest
from .common import CommandLineOptions
from .node_client import node_protocol_client
from .rnode import (bootstrap_connected_peer,
                    docker_network_with_started_bootstrap,
                    extract_validator_stake_from_bonds_validator_str)
from .wait import (wait_for_log_match,
                   wait_for_node_sees_block)

BOOTSTRAP_NODE_KEYS = conftest.KeyPair(private_key='ff2ba092524bafdbc85fa0c7eddb2b41c69bc9bf066a4711a8a16f749199e5be', public_key='0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb')
BONDED_VALIDATOR_KEY_1 = conftest.KeyPair(private_key='597623f0b50e82008d52644983699724538b4307efbe3d0b992c651ca7f860c8', public_key='04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519')
BONDED_VALIDATOR_KEY_2 = conftest.KeyPair(private_key='9a32ff7b7c6e25527e0b4e5bec70596c6094e6529d56bf61cbd1ca26d3e92b10', public_key='04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35')
BONDED_VALIDATOR_KEY_3 = conftest.KeyPair(private_key='af47862137d4e772f540029ae73ee01443c61288f3df9307a13d681de6ad2de4', public_key='04debd80cba7bff3f33cc8ca0520740060d3e770f6618d445c02746ea4cc320791b7aca71d9750b2805a8c930a9d568e46d5166f1d7502260e82e5e3a7a535b981')


def generate_block_hash() -> bytes:
    blake = hashlib.blake2b(digest_size=32)
    blake.update(b'evil')
    return blake.digest()


def hex_block_hash(block_hash: bytes) -> bytes:
    return base64.b16encode(block_hash).lower()


@pytest.mark.skipif(sys.platform in ('win32', 'cygwin', 'darwin'), reason="Only Linux docker support connection between host and container which node client needs")
def test_simple_slash(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    peers_keypairs = [BONDED_VALIDATOR_KEY_1, BONDED_VALIDATOR_KEY_2]
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=BOOTSTRAP_NODE_KEYS, peers_keypairs=peers_keypairs) as context, \
            docker_network_with_started_bootstrap(context=context) as bootstrap_node, \
            bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', keypair=BONDED_VALIDATOR_KEY_1) as validator1, \
            bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2', keypair=BONDED_VALIDATOR_KEY_2) as validator2, \
            node_protocol_client(bootstrap_node.network, docker_client) as client:
        contract = '/opt/docker/examples/tut-hello.rho'

        validator1.deploy(contract, BONDED_VALIDATOR_KEY_1.private_key)
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

        validator2.deploy(contract, BONDED_VALIDATOR_KEY_2.private_key)

        slashed_block_hash = validator2.propose()

        block_info = validator2.show_block_parsed(slashed_block_hash)
        bonds_validators = extract_validator_stake_from_bonds_validator_str(block_info['bondsValidatorList'])

        assert bonds_validators[BONDED_VALIDATOR_KEY_1.public_key] == 0
