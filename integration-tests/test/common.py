import os
import random
import string
import tempfile
from typing import (
    List,
    Tuple,
    Optional,
    TYPE_CHECKING,
)
import dataclasses
from rchain.crypto import PrivateKey

from docker.client import DockerClient

if TYPE_CHECKING:
    # pylint: disable=cyclic-import
    from .wait import PredicateProtocol


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
    bootstrap_key: PrivateKey
    peers_keys: List[PrivateKey]
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


class ParsingError(NonZeroExitCodeError):
    pass


class WaitTimeoutError(Exception):
    def __init__(self, predicate: 'PredicateProtocol', timeout: int) -> None:
        super().__init__()
        self.predicate = predicate
        self.timeout = timeout

class TransderFundsError(Exception):
    def __init__(self, reason: str) -> None:
        super().__init__()
        self.reason = reason


def random_string(context: TestingContext, length: int) -> str:
    return ''.join(context.random_generator.choice(string.ascii_letters) for m in range(length))


def make_tempfile(prefix: str, content: str) -> str:
    fd, path = tempfile.mkstemp(prefix=prefix)

    with os.fdopen(fd, 'w') as tmp:
        tmp.write(content)

    return path


def make_tempdir(prefix: str) -> str:
    return tempfile.mkdtemp(prefix=prefix)
