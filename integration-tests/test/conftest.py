import os
import time
from random import Random
import tempfile
import logging
import contextlib
from collections import defaultdict
from typing import (
    Any,
    List,
    Generator,
    Dict,
    Optional,
)

import pytest
from _pytest.terminal import TerminalReporter
from _pytest.reports import TestReport
from _pytest.config.argparsing import Parser
import docker as docker_py
from docker.client import DockerClient
from rchain.crypto import PrivateKey

from .common import (
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

def pytest_terminal_summary(terminalreporter:TerminalReporter) -> None:
    tr = terminalreporter
    dlist: Dict[str, List[TestReport]] = defaultdict(list)
    for replist in tr.stats.values():
        for rep in replist:
            if hasattr(rep, "duration"):
                dlist[rep.nodeid].append(rep)
    if not dlist:
        return
    tr.write_sep("=", "test durations")

    for nodeid, reps in dlist.items():
        total_second = sum([rep.duration for rep in reps])
        detail_duration = ",".join(["{:<8} {:8.2f}s".format(rep.when, rep.duration) for rep in reps])
        tr.write_line("Total: {:8.2f}s, {}    {}".format(total_second, detail_duration, nodeid))

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
def temporary_bonds_file(validator_bonds_dict: Dict[PrivateKey, int]) -> Generator[str, None, None]:
    (fd, file) = tempfile.mkstemp(prefix="rchain-bonds-file-", suffix=".txt", dir="/tmp")
    try:
        with os.fdopen(fd, "w") as f:
            for private_key, bond in validator_bonds_dict.items():
                f.write("{} {}\n".format(private_key.get_public_key().to_hex(), bond))
        yield file
    finally:
        os.unlink(file)


def make_wallets_file_lines(wallet_balance_from_private_key: Dict[PrivateKey, int]) -> List[str]:
    result = []
    for private_key, token_amount in wallet_balance_from_private_key.items():
        line = '{},{},0'.format(private_key.get_public_key().get_eth_address(), token_amount)
        result.append(line)
    return result

def generate_random_wallet_map(random_generator: Random, wallets_amount:int = 5) -> Dict[PrivateKey, int]:
    wallet_balance_from_private_key = {}
    for _  in range(wallets_amount):
        wallet_balance_from_private_key[PrivateKey.generate()] = random_generator.randint(10, 1000)
    return wallet_balance_from_private_key

@contextlib.contextmanager
def temporary_wallets_file(random_generator: Random, wallet_balance_from_private_key: Optional[Dict[PrivateKey, int]]= None) -> Generator[str, None, None]:
    if wallet_balance_from_private_key is None:
        wallet_balance_from_private_key = generate_random_wallet_map(random_generator)
    lines = make_wallets_file_lines(wallet_balance_from_private_key)
    (fd, file) = tempfile.mkstemp(prefix="rchain-wallets-file-", suffix=".txt")
    try:
        with os.fdopen(fd, "w") as f:
            f.writelines('{}\n'.format(l) for l in lines)
        yield file
    finally:
        os.unlink(file)


@contextlib.contextmanager
def docker_client_context() -> Generator[DockerClient, None, None]:
    docker_client = docker_py.from_env()
    try:
        yield docker_client
    finally:
        docker_client.volumes.prune()
        docker_client.networks.prune()

@pytest.yield_fixture(scope='session')
def docker_client() -> Generator[DockerClient, None, None]:
    with docker_client_context() as docker_cli:
        yield docker_cli


@pytest.yield_fixture(scope='session')
def random_generator(command_line_options: CommandLineOptions) -> Generator[Random, None, None]:
    random_seed = time.time() if command_line_options.random_seed is None else command_line_options.random_seed
    logging.critical("Using tests random number generator seed: %d", random_seed)
    random_generator = Random(random_seed)
    yield random_generator


@contextlib.contextmanager
def testing_context(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient, bootstrap_key: PrivateKey = None, peers_keys: List[PrivateKey] = None, network_peers: int = 2, validator_bonds_dict: Dict[PrivateKey, int] = None) -> Generator[TestingContext, None, None]:
    if bootstrap_key is None:
        bootstrap_key = PREGENERATED_KEYPAIRS[0]
    if peers_keys is None:
        peers_keys = PREGENERATED_KEYPAIRS[1:][:network_peers]


    if validator_bonds_dict is None:
        bonds_file_keys = [bootstrap_key] + peers_keys
        validator_bonds_dict = dict()
        for private_key in bonds_file_keys:
            validator_bonds_dict[private_key] = random_generator.randint(1, 100)

    with temporary_bonds_file(validator_bonds_dict) as bonds_file:
        context = TestingContext(
            bonds_file=bonds_file,
            bootstrap_key=bootstrap_key,
            peers_keys=peers_keys,
            docker=docker_client,
            node_startup_timeout=command_line_options.node_startup_timeout,
            network_converge_timeout=command_line_options.network_converge_timeout,
            receive_timeout=command_line_options.receive_timeout,
            command_timeout=command_line_options.command_timeout,
            mount_dir=command_line_options.mount_dir,
            random_generator=random_generator,
        )

        yield context


testing_context.__test__ = False # type: ignore


@pytest.yield_fixture(scope='module')
def started_standalone_bootstrap_node(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> Generator[Node, None, None]:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            yield bootstrap_node
