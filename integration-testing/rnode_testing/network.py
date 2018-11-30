import logging
import contextlib

from rnode_testing.rnode import create_peer_nodes

from typing import List, TYPE_CHECKING, Optional, Generator

if TYPE_CHECKING:
    from conftest import TestConfig, ValidatorsData
    from docker.client import DockerClient
    from rnode_testing.rnode import Node

class Network:
    def __init__(self, network: str, bootstrap: "Node", peers: List["Node"]) -> None:
        self.network = network
        self.bootstrap = bootstrap
        self.peers = peers
        self.nodes = [bootstrap] + peers


@contextlib.contextmanager
def start_network(config: "TestConfig", docker: "DockerClient", bootstrap: "Node", validators_data: "ValidatorsData", allowed_peers: Optional[List[str]] = None) -> Generator["Network", None, None]:
    logging.debug("Docker network = {}".format(bootstrap.network))

    peers = create_peer_nodes(
        docker_client=docker,
        bootstrap=bootstrap,
        network=bootstrap.network,
        bonds_file=validators_data.bonds_file,
        key_pairs=validators_data.peers_keys,
        rnode_timeout=config.rnode_timeout,
        allowed_peers=allowed_peers,
    )

    try:
        yield Network(network=bootstrap.network, bootstrap=bootstrap, peers=peers)
    finally:
        for peer in peers:
            peer.cleanup()
