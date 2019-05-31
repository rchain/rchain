import threading
from random import Random

from docker.client import DockerClient

from . import conftest
from .common import (
    CommandLineOptions,
)
from .rnode import (
    Node,
    bootstrap_connected_peer,
    docker_network_with_started_bootstrap,
)
from .wait import (
    wait_for_peers_count_at_least,
    wait_for_blocks_count_at_least,
)


class DeployThread(threading.Thread):
    def __init__(self, name: str, node: Node, contract: str, count: int, private_key: str) -> None:
        threading.Thread.__init__(self)
        self.name = name
        self.node = node
        self.contract = contract
        self.count = count
        self.private_key = private_key

    def run(self) -> None:
        for _ in range(self.count):
            self.node.deploy(self.contract, self.private_key)
            self.node.propose()


BOOTSTRAP_NODE_KEYS = conftest.KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2')
BONDED_VALIDATOR_KEY_1 = conftest.KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3')
BONDED_VALIDATOR_KEY_2 = conftest.KeyPair(private_key='f7bfb2b3f2be909dd50beac05bece5940b1e7266816d7294291a2ff66a5d660b', public_key='04e77428161f1b9a20c8179f8e5c7dddda39d218815e8ced4cb1a8488d4f29bae6f0e1d0b48387cc54f325d3eeecef0c2c98239f0ca7754bef7981891608ce0ed8')
BONDED_VALIDATOR_KEY_3 = conftest.KeyPair(private_key='2b173084083291ac6850cb734dffb69dfcb280aeb152f0d5be979bea7827c03a', public_key='04a4c196102d874100710f6a274ff91245b97205c2cc06fdb72895f93be28bf1c9e64aaf3cc52a12ea99c462f9e556a0927d2b5a620506df367be634738b953672')


def test_multiple_deploys_at_once(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    contract_path = '/opt/docker/examples/shortfast.rho'
    peers_keypairs = [BONDED_VALIDATOR_KEY_1, BONDED_VALIDATOR_KEY_2, BONDED_VALIDATOR_KEY_3]
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=BOOTSTRAP_NODE_KEYS, peers_keypairs=peers_keypairs) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', keypair=BONDED_VALIDATOR_KEY_1) as no1:
                with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2', keypair=BONDED_VALIDATOR_KEY_2) as no2:
                    with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-3', keypair=BONDED_VALIDATOR_KEY_3) as no3:
                        wait_for_peers_count_at_least(context, no1, 3)

                        deploy1 = DeployThread("node1", no1, contract_path, 1, BONDED_VALIDATOR_KEY_1.private_key)
                        deploy1.start()

                        expected_blocks_count = 1
                        wait_for_blocks_count_at_least(
                            context,
                            no1,
                            expected_blocks_count,
                        )

                        deploy2 = DeployThread("node2", no2, contract_path, 3, BONDED_VALIDATOR_KEY_2.private_key)
                        deploy2.start()

                        deploy3 = DeployThread("node3", no3, contract_path, 3, BONDED_VALIDATOR_KEY_3.private_key)
                        deploy3.start()

                        expected_blocks_count = 7
                        wait_for_blocks_count_at_least(
                            context,
                            no1,
                            expected_blocks_count,
                        )
                        wait_for_blocks_count_at_least(
                            context,
                            no2,
                            expected_blocks_count,
                        )
                        wait_for_blocks_count_at_least(
                            context,
                            no3,
                            expected_blocks_count,
                        )

                        deploy1.join()
                        deploy2.join()
                        deploy3.join()
