import os
import random
import string
import typing
import tempfile
import dataclasses

from docker.client import DockerClient



@dataclasses.dataclass(eq=True, frozen=True)
class KeyPair:
    private_key: str
    public_key: str


@dataclasses.dataclass
class TestingContext:
    peer_count: int
    node_startup_timeout: int
    network_converge_timeout: int
    receive_timeout: int
    command_timeout: int
    blocks: int
    mount_dir: str
    bonds_file: str
    bootstrap_keypair: KeyPair
    peers_keypairs: typing.List[KeyPair]
    docker: 'DockerClient'


def random_string(length: int) -> str:
    return ''.join(random.choice(string.ascii_letters) for m in range(length))


def make_tempfile(prefix: str, content: str) -> str:
    fd, path = tempfile.mkstemp(dir="/tmp", prefix=prefix)

    with os.fdopen(fd, 'w') as tmp:
        tmp.write(content)

    return path


def make_tempdir(prefix: str) -> str:
    return tempfile.mkdtemp(dir="/tmp", prefix=prefix)



class Network:
    def __init__(self, network, bootstrap, peers):
        self.network = network
        self.bootstrap = bootstrap
        self.peers = peers
        self.nodes = [bootstrap] + peers
