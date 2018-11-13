import sys
sys.path.insert(0, '.')

import os
import pprint
import random
import logging
import tempfile
import contextlib
import collections

import pytest
import docker as docker_py

from rnode_testing.util import log_box
from rnode_testing.profiling import log_prof_data
from rnode_testing.rnode import start_bootstrap
import rnode_testing.resources as resources


System = collections.namedtuple("System", ["config", "docker", "validators_data"])


TestConfig = collections.namedtuple("TestConfig", [
    "peer_count",
    "node_startup_timeout",
    "network_converge_timeout",
    "receive_timeout",
    "rnode_timeout",
    "blocks",
])


KeyPair = collections.namedtuple("KeyPair", ["private_key", "public_key"])


ValidatorsData = collections.namedtuple("ValidatorsData", ["bonds_file", "bootstrap_keys", "peers_keys"])


def make_test_config(request):
    peer_count = int(request.config.getoption("--peer-count"))
    start_timeout = int(request.config.getoption("--start-timeout"))
    converge_timeout = int(request.config.getoption("--converge-timeout"))
    receive_timeout = int(request.config.getoption("--receive-timeout"))
    rnode_timeout = int(request.config.getoption("--rnode-timeout"))
    blocks = int(request.config.getoption("--blocks"))

    def make_timeout(value, base, peer_factor=10): return value if value > 0 else base + peer_count * peer_factor

    config = TestConfig(
        peer_count=peer_count,
        node_startup_timeout=make_timeout(start_timeout, 30, 10),
        network_converge_timeout=make_timeout(converge_timeout, 200, 10),
        receive_timeout=make_timeout(receive_timeout, 10, 10),
        rnode_timeout=rnode_timeout,
        blocks=blocks,
    )

    with log_box(logging.info):
        s = pprint.pformat(dict(config._asdict()), indent=4)
        logging.info(f"Running with test configuration: {s}")

    return config


@contextlib.contextmanager
def temporary_bonds_file(validator_keys):
    (fd, file) = tempfile.mkstemp(prefix="rchain-bonds-file-", suffix=".txt", dir="/tmp")
    try:
        with os.fdopen(fd, "w") as f:
            for pair in validator_keys:
                bond = random.randint(1, 100)
                f.write(f"{pair.public_key} {bond}\n")
        yield file
    finally:
        os.unlink(file)


@contextlib.contextmanager
def validators_data(config):
    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    lines = resources.file_content('pregenerated-validator-private-public-key-pairs.txt').splitlines()
    random.shuffle(lines)
    validator_keys = [KeyPair(*line.split()) for line in lines[0:config.peer_count+1]]
    with temporary_bonds_file(validator_keys) as f:
        yield ValidatorsData(bonds_file=f, bootstrap_keys=validator_keys[0], peers_keys=validator_keys[1:])


def pytest_addoption(parser):
    parser.addoption(
        "--peer-count", action="store", default="2", help="number of peers in the network (excluding bootstrap node)"
    )
    parser.addoption(
        "--start-timeout", action="store", default="0", help="timeout in seconds for starting a node. Defaults to 30 + peer_count * 10"
    )
    parser.addoption(
        "--converge-timeout", action="store", default="0", help="timeout in seconds for network converge. Defaults to 200 + peer_count * 10"
    )
    parser.addoption(
        "--receive-timeout", action="store", default="0", help="timeout in seconds for receiving a message. Defaults to 10 + peer_count * 10"
    )
    parser.addoption(
        "--rnode-timeout", action="store", default="10", help="timeout in seconds for executing an rnode call (Examples: propose, show-logs etc.). Defaults to 10s"
    )
    parser.addoption(
        "--blocks", action="store", default="1", help="The number of deploys per test deploy"
    )


@pytest.yield_fixture(scope='session')
def docker_client_session():
    docker_client = docker_py.from_env()
    try:
        yield docker_client
    finally:
        docker_client.volumes.prune()
        docker_client.networks.prune()



@pytest.yield_fixture(scope="session")
def system(request, docker_client_session):
    cfg = make_test_config(request)
    with validators_data(cfg) as vd:
        try:
            yield System(cfg, docker_client_session, vd)
        finally:
            log_prof_data()


@pytest.yield_fixture(scope="module")
def bootstrap_node(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data) as node:
        yield node
