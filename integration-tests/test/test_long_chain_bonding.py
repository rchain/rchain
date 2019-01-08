from docker.client import DockerClient

import threading

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
BONDED_VALIDATOR_KEYPAIR  = KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821')
BONDED_VALIDATOR_KEYPAIR2 = KeyPair(private_key='415e0da84949fc64e92dfd8114d5647f31d38e5b36a6d98416891b46e176b095', public_key='f6963b6c1ca127e945d391e50ac1896b0a298ef7495ee55d32b84e462ef6f050')
BONDED_VALIDATOR_KEYPAIR3 = KeyPair(private_key='b37d119d584232ee9c1ace08d3dde80c7d87b1cf441bba68cd252e83a607410b', public_key='f7382d55a40fae6614da8f25ade5fc90e3439d02a74b576bb762936dab3ed021')
BONDED_VALIDATOR_KEYPAIR4 = KeyPair(private_key='53d9c017de6a5250260d08356e3e98191b2d504ad27e81dd499e614b55d89764', public_key='f7499d2ba3e6672b1058b5196fa1a83fa529c431643ad2902aef7fd39f742db7')
BONDED_VALIDATOR_KEYPAIR5 = KeyPair(private_key='cd76fbc9e3f3b50dcbc096538acb37ef21ca7e5d3c7801c563ef73fe6439eb83', public_key='f86d67e5ed0c9956a83e72f11106da781460170ae4ae5d7381f5a46eba4feeb3')
JOINING_VALIDATOR_KEYPAIR = KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e')
READONLY_PEER_KEYPAIR = KeyPair(private_key='2bdedd2e4dd2e7b5f176b7a5bc155f10fafd3fbd9c03fb7556f2ffd22c786f8b', public_key='068e8311fe094e1a33646a1f8dfb50a5c12b49a1e5d0cf4cccf28d31b4a10255')

import random
import os

class DeployRandomThread(threading.Thread):
    def __init__(self, name, node, count):
        threading.Thread.__init__(self)
        self.name = name
        self.node = node
        self.relative_paths = self.node.shell_out('sh', '-c', 'ls /opt/docker/examples/shortfast.rho').splitlines()
        self.count = count

    def run(self):
        for _ in range(self.count):
            full_path = os.path.join('/opt/docker/examples', random.choice(self.relative_paths))
            self.node.deploy(full_path)
            self.node.propose()

def test_long_chain_bonding(command_line_options: CommandLineOptions, docker_client: DockerClient):
    BONDED_VALIDATOR_BLOCKS = JOINING_VALIDATOR_BLOCKS = 30
    with conftest.testing_context(command_line_options, docker_client, bootstrap_keypair=BOOTSTRAP_NODE_KEYPAIR, peers_keypairs=[BONDED_VALIDATOR_KEYPAIR, BONDED_VALIDATOR_KEYPAIR2, BONDED_VALIDATOR_KEYPAIR3, BONDED_VALIDATOR_KEYPAIR4, BONDED_VALIDATOR_KEYPAIR5]) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator', keypair=BONDED_VALIDATOR_KEYPAIR) as bonded_validator:
                with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator2', keypair=BONDED_VALIDATOR_KEYPAIR2) as bonded_validator2:
                    with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator3', keypair=BONDED_VALIDATOR_KEYPAIR3) as bonded_validator3:
                    #     with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator4', keypair=BONDED_VALIDATOR_KEYPAIR4) as bonded_validator4:
                    #         with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator5', keypair=BONDED_VALIDATOR_KEYPAIR5) as bonded_validator5:
                                peer_amount = 3
                                wait_for_peers_count_at_least(context, bonded_validator, peer_amount)
                                contract_path = '/opt/docker/examples/hello_world_again.rho'
                                non_conflicting_contract_path = '/opt/docker/examples/shortfast.rho'

                                deploy1 = DeployRandomThread("node1", bonded_validator, 3)
                                deploy1.start()

                                expected_blocks_count = 3
                                wait_for_blocks_count_at_least(
                                    context,
                                    bonded_validator,
                                    expected_blocks_count,
                                )
                                deploy2 = DeployRandomThread("node2", bonded_validator2, 100)
                                deploy2.start()

                                deploy3 = DeployRandomThread("node3", bonded_validator3, 30)
                                deploy3.start()

                                # deploy4 = DeployRandomThread("node4", bonded_validator4, 30)
                                # deploy4.start()

                                # deploy5 = DeployRandomThread("node5", bonded_validator5, 30)
                                # deploy5.start()

                                with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='joining-validator', keypair=JOINING_VALIDATOR_KEYPAIR) as joining_validator:
                                    joining_validator.generate_faucet_bonding_deploys(
                                        bond_amount=123,
                                        private_key=JOINING_VALIDATOR_KEYPAIR.private_key,
                                        public_key=JOINING_VALIDATOR_KEYPAIR.public_key,
                                    )
                                    wait_for_peers_count_at_least(context, joining_validator, peer_amount + 1)
                                    forward_file = joining_validator.cat_forward_file(public_key=JOINING_VALIDATOR_KEYPAIR.public_key)
                                    bond_file = joining_validator.cat_bond_file(public_key=JOINING_VALIDATOR_KEYPAIR.public_key)
                                    bonded_validator.deploy_string(forward_file)
                                    bonded_validator.propose()
                                    bonded_validator.deploy_string(bond_file)
                                    bonding_block_hash = bonded_validator.propose()
                                    bonded_validator.deploy(non_conflicting_contract_path)
                                    post_bonding_block_hash = bonded_validator.propose()

                                    wait_for_node_sees_block(context, bonded_validator2, post_bonding_block_hash)
                                    wait_for_node_sees_block(context, joining_validator, post_bonding_block_hash)
                                    
                                    for _ in range(JOINING_VALIDATOR_BLOCKS):
                                        joining_validator.deploy(contract_path)
                                        joining_validator.propose()

                                    with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='readonly-peer', keypair=READONLY_PEER_KEYPAIR) as readonly_peer:
                                        wait_for_peers_count_at_least(context, bonded_validator, peer_amount + 2)
                                        # Force sync with the network
                                        joining_validator.deploy(contract_path)
                                        joining_validator.propose()
                                        expected_blocks_count = 130
                                        wait_for_blocks_count_at_least(
                                            context,
                                            readonly_peer,
                                            expected_blocks_count,
                                        )
