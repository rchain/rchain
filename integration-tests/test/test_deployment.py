import pytest

@pytest.mark.xfail
def test_simple_deploy(command_line_options_fixture, docker_client_fixture):
    assert False

@pytest.mark.xfail
def test_incorrect_contract_does_not_deploy(command_line_options_fixture, docker_client_fixture):
    assert False
