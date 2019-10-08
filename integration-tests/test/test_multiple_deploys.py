import threading
from random import Random

from docker.client import DockerClient
from rchain.crypto import PrivateKey

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
    def __init__(self, name: str, node: Node, contract: str, count: int, private_key: PrivateKey) -> None:
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


BOOTSTRAP_NODE_KEYS = PrivateKey.from_hex("80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709")
BONDED_VALIDATOR_KEY_1 = PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5")
BONDED_VALIDATOR_KEY_2 = PrivateKey.from_hex("f7bfb2b3f2be909dd50beac05bece5940b1e7266816d7294291a2ff66a5d660b")
BONDED_VALIDATOR_KEY_3 = PrivateKey.from_hex("2b173084083291ac6850cb734dffb69dfcb280aeb152f0d5be979bea7827c03a")


def test_multiple_deploys_at_once(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    contract_path = '/opt/docker/examples/shortfast.rho'
    peers_keypairs = [BONDED_VALIDATOR_KEY_1, BONDED_VALIDATOR_KEY_2, BONDED_VALIDATOR_KEY_3]
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_key=BOOTSTRAP_NODE_KEYS, peers_keys=peers_keypairs) as context, \
        docker_network_with_started_bootstrap(context=context) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', private_key=BONDED_VALIDATOR_KEY_1) as no1, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2', private_key=BONDED_VALIDATOR_KEY_2) as no2, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-3', private_key=BONDED_VALIDATOR_KEY_3) as no3:
            wait_for_peers_count_at_least(context, no1, 3)

            deploy1 = DeployThread("node1", no1, contract_path, 1, BONDED_VALIDATOR_KEY_1)
            deploy1.start()

            expected_blocks_count = 1
            wait_for_blocks_count_at_least(
                context,
                no1,
                expected_blocks_count,
            )

            deploy2 = DeployThread("node2", no2, contract_path, 3, BONDED_VALIDATOR_KEY_2)
            deploy2.start()

            deploy3 = DeployThread("node3", no3, contract_path, 3, BONDED_VALIDATOR_KEY_3)
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
