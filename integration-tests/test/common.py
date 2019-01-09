import os
import random
import string
import tempfile
from typing import (
    List,
    Tuple,
    Optional,
)
import dataclasses

from docker.client import DockerClient


@dataclasses.dataclass(eq=True, frozen=True)
class KeyPair:
    private_key: str
    public_key: str


@dataclasses.dataclass
class CommandLineOptions:
    node_startup_timeout: int
    network_converge_timeout: int
    receive_timeout: int
    command_timeout: int
    mount_dir: str
    random_seed: Optional[int]


@dataclasses.dataclass
class TestingContext:
    # Tell pytest to ignore this class (produces warnings otherwise)
    __test__ = False

    node_startup_timeout: int
    network_converge_timeout: int
    receive_timeout: int
    command_timeout: int
    mount_dir: str
    bonds_file: str
    bootstrap_keypair: KeyPair
    peers_keypairs: List[KeyPair]
    docker: DockerClient
    random_generator: random.Random


class NonZeroExitCodeError(Exception):
    def __init__(self, command: Tuple[str, ...], exit_code: int, output: str) -> None:
        super().__init__()
        self.command = command
        self.exit_code = exit_code
        self.output = output


class GetBlockError(NonZeroExitCodeError):
    pass


def random_string(context: TestingContext, length: int) -> str:
    return ''.join(context.random_generator.choice(string.ascii_letters) for m in range(length))


def make_tempfile(prefix: str, content: str) -> str:
    fd, path = tempfile.mkstemp(dir="/tmp", prefix=prefix)

    with os.fdopen(fd, 'w') as tmp:
        tmp.write(content)

    return path


def make_tempdir(prefix: str) -> str:
    return tempfile.mkdtemp(dir="/tmp", prefix=prefix)
