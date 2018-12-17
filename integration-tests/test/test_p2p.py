import os
 from .conftest import testing_context
from .rnode import docker_network_with_started_bootstrap
from .wait import (
    wait_for_approved_block_received_handler_state,
)
import pytest

@pytest.mark.xfail
def test_connecting_to_existing_node(command_line_options_fixture, docker_client_fixture):
    fail 

@pytest.mark.xfail
def test_connecting_to_non_existing_node(command_line_options_fixture, docker_client_fixture):
    fail 
