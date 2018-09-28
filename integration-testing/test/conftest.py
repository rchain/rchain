import logging
import pytest


import random
import tempfile
from tools.util import log_box
from tools.profiling import log_prof_data
import tools.resources as resources
import os
import pprint
from contextlib import contextmanager
import collections

Config = collections.namedtuple( "Config",
                                 [
                                     "peer_count",
                                     "node_startup_timeout",
                                     "network_converge_timeout",
                                     "receive_timeout",
                                     "rnode_timeout",
                                     "blocks"
                                 ])


KeyPair = collections.namedtuple("KeyPair", ["private_key", "public_key"])

ValidatorsData = collections.namedtuple("ValidatorsData", ["bonds_file", "bootstrap_keys", "peers_keys"])

def parse_config(request):
    peer_count = int(request.config.getoption("--peer-count"))
    start_timeout = int(request.config.getoption("--start-timeout"))
    converge_timeout = int(request.config.getoption("--converge-timeout"))
    receive_timeout = int(request.config.getoption("--receive-timeout"))
    rnode_timeout = int(request.config.getoption("--rnode-timeout"))
    blocks = int(request.config.getoption("--blocks"))

    def make_timeout(value, base, peer_factor=10): return value if value > 0 else base + peer_count * peer_factor

    config = Config( peer_count = peer_count,
                     node_startup_timeout = make_timeout(start_timeout, 30, 10),
                     network_converge_timeout = make_timeout(converge_timeout, 200, 10),
                     receive_timeout = make_timeout(receive_timeout, 10, 10),
                     rnode_timeout = rnode_timeout,
                     blocks = blocks
                     )

    with log_box(logging.info):
        s = pprint.pformat(dict(config._asdict()), indent=4)
        logging.info(f"Running with test configuration: {s}")

    return config


@pytest.fixture(scope="session")
def config(request):
    try:
        yield parse_config(request)

    finally:
        log_prof_data()

@pytest.fixture(scope="session")
def docker():
    import docker

    docker_client = docker.from_env()

    try:
        yield docker_client
    finally:
        logging.info("Remove unused volumes")
        docker_client.volumes.prune()



@contextmanager
def bonds_file(validator_keys):
    (fd, file) = tempfile.mkstemp(prefix="rchain-bonds-file-", suffix = ".txt", dir="/tmp")
    logging.info(f"Using bonds file: `{bonds_file}`")

    with os.fdopen(fd, "w") as f:
        for pair in validator_keys:
            bond = random.randint(1, 100)
            f.write(f"{pair.public_key} {bond}\n")

    try:
        yield file
    finally:
        os.unlink(file)
        logging.info(f"Bonds file `{bonds_file}` deleted")

@pytest.fixture(scope="session")
def validators_data(config):
    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    f=open(resources.file_path('pregenerated-validator-private-public-key-pairs.txt'))
    lines=f.readlines()
    random.shuffle(lines)
    validator_keys = [ KeyPair(*line.split())
                       for line in lines[0:config.peer_count+1]]

    logging.info(f"Using validator keys: {validator_keys}")

    with bonds_file(validator_keys) as f:
        yield ValidatorsData(f, validator_keys[0], validator_keys[1:])



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