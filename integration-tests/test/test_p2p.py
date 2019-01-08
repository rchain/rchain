import pytest
from docker.client import DockerClient

from .common import (
    CommandLineOptions,
)


@pytest.mark.xfail
def test_connecting_to_existing_node(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_connecting_to_non_existing_node(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_discover_other_nodes(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_number_of_protocol_peers(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_number_of_discovery_peers(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False
