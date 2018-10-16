import logging
import random
import tempfile
import rnode_testing.resources as resources
import os
import pprint
from contextlib import contextmanager
import collections


@contextmanager
def log_box(log_function, title="", char="*", length=150):
    full_title = f" {title} " if title else ""
    title_stars_len = int((length - len(full_title)) / 2)
    title_stars = char * title_stars_len
    log_function(title_stars + full_title + title_stars)
    try:
        yield
    finally:
        log_function(char * length)


def make_tempfile(prefix, content):
    fd, path = tempfile.mkstemp(dir="/tmp", prefix=prefix)

    with os.fdopen(fd, 'w') as tmp:
        tmp.write(content)


def make_tempdir(prefix):
    return tempfile.mkdtemp(dir="/tmp", prefix=prefix)


Config = collections.namedtuple("Config",
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

    config = Config(peer_count=peer_count,
                    node_startup_timeout=make_timeout(start_timeout, 30, 10),
                    network_converge_timeout=make_timeout(converge_timeout, 200, 10),
                    receive_timeout=make_timeout(receive_timeout, 10, 10),
                    rnode_timeout=rnode_timeout,
                    blocks=blocks
                    )

    with log_box(logging.info):
        s = pprint.pformat(dict(config._asdict()), indent=4)
        logging.info(f"Running with test configuration: {s}")

    return config


@contextmanager
def docker():
    import docker

    docker_client = docker.from_env()

    try:
        yield docker_client
    finally:
        logging.info("Remove unused volumes")
        docker_client.volumes.prune()

        logging.info("Remove unused networks")
        docker_client.networks.prune()


@contextmanager
def bonds_file(validator_keys):
    (fd, file) = tempfile.mkstemp(prefix="rchain-bonds-file-", suffix=".txt", dir="/tmp")

    try:
        logging.info(f"Using bonds file: `{file}`")

        with os.fdopen(fd, "w") as f:
            for pair in validator_keys:
                bond = random.randint(1, 100)
                f.write(f"{pair.public_key} {bond}\n")
        yield file
    finally:
        os.unlink(file)
        logging.info(f"Bonds file `{file}` deleted")


@contextmanager
def validators_data(config):
    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    lines = resources.file_content('pregenerated-validator-private-public-key-pairs.txt').splitlines()

    random.shuffle(lines)

    validator_keys = [KeyPair(*line.split())
                      for line in lines[0:config.peer_count+1]]

    logging.info(f"Using validator keys: {validator_keys}")

    with bonds_file(validator_keys) as f:
        yield ValidatorsData(bonds_file=f, bootstrap_keys=validator_keys[0], peers_keys=validator_keys[1:])
