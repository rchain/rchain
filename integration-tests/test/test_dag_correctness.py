import pytest
from docker.client import DockerClient

from .common import (
    CommandLineOptions,
)


@pytest.mark.xfail
def test_fault_tolerance(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_catch_up_next_round(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_catch_up(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False
