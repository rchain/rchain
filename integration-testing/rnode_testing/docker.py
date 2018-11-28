import logging
from contextlib import contextmanager
import rnode_testing.random

from docker.client import DockerClient
from typing import Generator

@contextmanager
def docker_network(docker_client: DockerClient) -> Generator[str, None, None]:
    network_name = "rchain-{}".format(rnode_testing.random.random_string(5).lower())

    docker_client.networks.create(network_name, driver="bridge")

    try:
        yield network_name
    finally:
        for network in docker_client.networks.list():
            if network_name == network.name:
                logging.info("Removing docker network {}".format(network.name))
                network.remove()
