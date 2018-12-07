import sys

sys.path.insert(0, '.')

import os
import random
import pathlib
import tempfile
import logging
import contextlib
import collections
import dataclasses
from typing import (
    TYPE_CHECKING,
    List,
    Generator,
    TextIO,
)

import pytest
import docker as docker_py

from rnode_testing.common import (
    KeyPair,
    TestingContext,
)
from rnode_testing.pregenerated_keypairs import PREGENERATED_KEYPAIRS

if TYPE_CHECKING:
    from docker.client import DockerClient
    from _pytest.config.argparsing import Parser


# Silence unwanted noise in logs produced at the DEBUG level
logging.getLogger('urllib3.connectionpool').setLevel(logging.WARNING)
logging.getLogger('connectionpool.py').setLevel(logging.WARNING)
logging.getLogger('docker.utils.config').setLevel(logging.WARNING)
logging.getLogger('docker.auth').setLevel(logging.WARNING)



@dataclasses.dataclass
class CommandLineOptions:
    node_startup_timeout: int
    network_converge_timeout: int
    receive_timeout: int
    command_timeout: int
    mount_dir: str


def pytest_addoption(parser: "Parser") -> None:
    parser.addoption("--startup-timeout", type=int, action="store", default=60 * 30, help="timeout in seconds for starting a node")
    parser.addoption("--converge-timeout", type=int, action="store", default=60 * 30, help="timeout in seconds for network converge")
    parser.addoption("--receive-timeout", type=int, action="store", default=60 * 30, help="timeout in seconds for receiving a message")
    parser.addoption("--command-timeout", type=int, action="store", default=60 * 30, help="timeout in seconds for executing a rnode call")
    parser.addoption("--mount-dir", action="store", default=None, help="globally accesible directory for mounting between containers")


@pytest.yield_fixture(scope='session')
def command_line_options_fixture(request):
    startup_timeout = int(request.config.getoption("--startup-timeout"))
    converge_timeout = int(request.config.getoption("--converge-timeout"))
    receive_timeout = int(request.config.getoption("--receive-timeout"))
    command_timeout = int(request.config.getoption("--command-timeout"))
    mount_dir = request.config.getoption("--mount-dir")

    command_line_options = CommandLineOptions(
        node_startup_timeout=startup_timeout,
        network_converge_timeout=converge_timeout,
        receive_timeout=receive_timeout,
        command_timeout=command_timeout,
        mount_dir=mount_dir,
    )

    yield command_line_options



@contextlib.contextmanager
def temporary_bonds_file(validator_keys: List[KeyPair]) -> Generator[str, None, None]:
    (fd, file) = tempfile.mkstemp(prefix="rchain-bonds-file-", suffix=".txt", dir="/tmp")
    try:
        with os.fdopen(fd, "w") as f:
            for pair in validator_keys:
                bond = random.randint(1, 100)
                f.write("{} {}\n".format(pair.public_key, bond))
        yield file
    finally:
        os.unlink(file)


@pytest.yield_fixture(scope='session')
def docker_client_fixture() -> Generator["DockerClient", None, None]:
    docker_client = docker_py.from_env()
    try:
        yield docker_client
    finally:
        docker_client.volumes.prune()
        docker_client.networks.prune()


@contextlib.contextmanager
def testing_context(command_line_options_fixture, docker_client_fixture, bootstrap_keypair: KeyPair = None, peers_keypairs: List[KeyPair] = None, network_peers: int = 2) -> Generator[TestingContext, None, None]:
    if bootstrap_keypair is None:
        bootstrap_keypair = PREGENERATED_KEYPAIRS[0]
    if peers_keypairs is None:
        peers_keypairs = PREGENERATED_KEYPAIRS[1:][:network_peers]

    bonds_file_keypairs = [bootstrap_keypair] + peers_keypairs
    with temporary_bonds_file(bonds_file_keypairs) as bonds_file:
        context = TestingContext(
            bonds_file=bonds_file,
            bootstrap_keypair=bootstrap_keypair,
            peers_keypairs=peers_keypairs,
            docker=docker_client_fixture,
            **dataclasses.asdict(command_line_options_fixture),
        )

        yield context
