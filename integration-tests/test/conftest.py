import os
import time
from random import Random
import tempfile
import logging
import contextlib
from typing import (
    Any,
    List,
    Generator,
)

import pytest
from _pytest.config.argparsing import Parser
import docker as docker_py
from docker.client import DockerClient

from .common import (
    KeyPair,
    CommandLineOptions,
    TestingContext,
)
from .rnode import (
    Node,
    docker_network_with_started_bootstrap,
)
from .pregenerated_keypairs import PREGENERATED_KEYPAIRS


# Silence unwanted noise in logs produced at the DEBUG level
logging.getLogger('urllib3.connectionpool').setLevel(logging.WARNING)
logging.getLogger('connectionpool.py').setLevel(logging.WARNING)
logging.getLogger('docker.utils.config').setLevel(logging.WARNING)
logging.getLogger('docker.auth').setLevel(logging.WARNING)


def pytest_addoption(parser: Parser) -> None:
    parser.addoption("--startup-timeout", type=int, action="store", default=60 * 30, help="timeout in seconds for starting a node")
    parser.addoption("--converge-timeout", type=int, action="store", default=60 * 30, help="timeout in seconds for network converge")
    parser.addoption("--receive-timeout", type=int, action="store", default=30, help="timeout in seconds for receiving a single block")
    parser.addoption("--command-timeout", type=int, action="store", default=60 * 30, help="timeout in seconds for executing a rnode call")
    parser.addoption("--mount-dir", action="store", default=None, help="globally accesible directory for mounting between containers")
    parser.addoption("--random-seed", type=int, action="store", default=None, help="seed for the random numbers generator used in integration tests")


@pytest.yield_fixture(scope='session')
def command_line_options(request: Any) -> Generator[CommandLineOptions, None, None]:
    startup_timeout = int(request.config.getoption("--startup-timeout"))
    converge_timeout = int(request.config.getoption("--converge-timeout"))
    receive_timeout = int(request.config.getoption("--receive-timeout"))
    command_timeout = int(request.config.getoption("--command-timeout"))
    mount_dir = request.config.getoption("--mount-dir")
    random_seed = request.config.getoption("--random-seed")

    command_line_options = CommandLineOptions(
        node_startup_timeout=startup_timeout,
        network_converge_timeout=converge_timeout,
        receive_timeout=receive_timeout,
        command_timeout=command_timeout,
        mount_dir=mount_dir,
        random_seed=random_seed,
    )

    yield command_line_options


@contextlib.contextmanager
def temporary_bonds_file(random_generator: Random, validator_keys: List[KeyPair]) -> Generator[str, None, None]:
    (fd, file) = tempfile.mkstemp(prefix="rchain-bonds-file-", suffix=".txt")
    try:
        with os.fdopen(fd, "w") as f:
            for pair in validator_keys:
                bond = random_generator.randint(1, 100)
                f.write("{} {}\n".format(pair.public_key, bond))
        yield file
    finally:
        os.unlink(file)


def make_wallets_file_lines(random_generator: Random, validator_keys: List[KeyPair]) -> List[str]:
    result = []
    for keypair in validator_keys:
        token_amount = random_generator.randint(1, 100)
        line = '0x{},{},0'.format(keypair.public_key, token_amount)
        result.append(line)
    return result


@contextlib.contextmanager
def temporary_wallets_file(random_generator: Random, validator_keys: List[KeyPair]) -> Generator[str, None, None]:
    lines = make_wallets_file_lines(random_generator, validator_keys)
    (fd, file) = tempfile.mkstemp(prefix="rchain-wallets-file-", suffix=".txt")
    try:
        with os.fdopen(fd, "w") as f:
            f.writelines('{}\n'.format(l) for l in lines)
        yield file
    finally:
        os.unlink(file)


@pytest.yield_fixture(scope='session')
def docker_client() -> Generator[DockerClient, None, None]:
    docker_client = docker_py.from_env()
    try:
        yield docker_client
    finally:
        docker_client.volumes.prune()
        docker_client.networks.prune()


@pytest.yield_fixture(scope='session')
def random_generator(command_line_options: CommandLineOptions) -> Generator[Random, None, None]:
    random_seed = time.time() if command_line_options.random_seed is None else command_line_options.random_seed
    logging.critical("Using tests random number generator seed: %d", random_seed)
    random_generator = Random(random_seed)
    yield random_generator


@contextlib.contextmanager
def testing_context(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient, bootstrap_keypair: KeyPair = None, peers_keypairs: List[KeyPair] = None, network_peers: int = 2) -> Generator[TestingContext, None, None]:
    if bootstrap_keypair is None:
        bootstrap_keypair = PREGENERATED_KEYPAIRS[0]
    if peers_keypairs is None:
        peers_keypairs = PREGENERATED_KEYPAIRS[1:][:network_peers]

    bonds_file_keypairs = [bootstrap_keypair] + peers_keypairs
    with temporary_bonds_file(random_generator, bonds_file_keypairs) as bonds_file:
        context = TestingContext(
            bonds_file=bonds_file,
            bootstrap_keypair=bootstrap_keypair,
            peers_keypairs=peers_keypairs,
            docker=docker_client,
            node_startup_timeout=command_line_options.node_startup_timeout,
            network_converge_timeout=command_line_options.network_converge_timeout,
            receive_timeout=command_line_options.receive_timeout,
            command_timeout=command_line_options.command_timeout,
            mount_dir=command_line_options.mount_dir,
            random_generator=random_generator,
        )

        yield context


testing_context.__test__ = False  # type: ignore


@pytest.yield_fixture(scope='module')
def started_standalone_bootstrap_node(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> Generator[Node, None, None]:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            yield bootstrap_node
