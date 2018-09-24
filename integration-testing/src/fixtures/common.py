import logging
import pytest

import random
import tools.random
import tempfile
from tools.util import log_box
from tools.profiling import log_prof_data
import tools.resources as resources
import os
import pprint
from contextlib import contextmanager
from fixtures.datastructures import Config, KeyPair


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
    yield parse_config(request)
    log_prof_data()

@pytest.fixture(scope="session")
def docker():
    import docker

    docker_client = docker.from_env()

    yield docker_client

    logging.info("Remove unused volumes")
    docker_client.volumes.prune()


@pytest.fixture(scope="package")
def docker_network(docker):
    network_name = f"rchain-{tools.random.random_string(5).lower()}"

    docker.networks.create(network_name, driver="bridge")

    yield network_name

    for network in docker.networks.list():
        if network_name == network.name:
            logging.info(f"removing {network.name}")
            network.remove()


@contextmanager
def create_bonds_file(validator_keys):
    (fd, bonds_file) = tempfile.mkstemp(prefix="rchain-bonds-file-", suffix = ".txt", dir="/tmp")
    logging.info(f"Using bonds file: `{bonds_file}`")

    with os.fdopen(fd, "w") as f:
        for pair in validator_keys:
            bond = random.randint(1, 100)
            f.write(f"{pair.public_key} {bond}\n")

    yield bonds_file

    os.unlink(bonds_file)
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

    with create_bonds_file(validator_keys) as bonds_file:
        yield (bonds_file, validator_keys[0], validator_keys[1:])
