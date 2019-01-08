import pytest
from docker.client import DockerClient

from .common import (
    CommandLineOptions,
)


@pytest.mark.xfail
def test_data_is_stored_and_served_by_node(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False
