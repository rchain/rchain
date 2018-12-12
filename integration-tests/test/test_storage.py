import os

from .conftest import testing_context
from .rnode import docker_network_with_started_bootstrap
from .wait import (
    wait_for_approved_block_received_handler_state,
)


@pytest.mark.xfail
def test_non_transactional_data_is_stored_and_served_by_node(command_line_options_fixture, docker_client_fixture):
    fail