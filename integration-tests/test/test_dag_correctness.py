import pytest

@pytest.mark.xfail
def test_fault_tolerance(command_line_options_fixture, docker_client_fixture):
    assert False

@pytest.mark.xfail
def test_catch_up_next_round(command_line_options_fixture, docker_client_fixture):
    assert False

@pytest.mark.xfail
def test_catch_up(command_line_options_fixture, docker_client_fixture):
    assert False
