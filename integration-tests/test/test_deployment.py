import pytest
from docker.client import DockerClient

from .common import (
    CommandLineOptions,
)


@pytest.mark.xfail
def test_simple_deploy(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_incorrect_contract_does_not_deploy(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False
