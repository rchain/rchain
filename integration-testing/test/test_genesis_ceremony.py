import contextlib
from typing import (
    Generator,
    List,
    TYPE_CHECKING,
)

import pytest

import conftest
from rnode_testing.rnode import docker_network_with_started_bootstrap

if TYPE_CHECKING:
    from _pytest.fixtures import FixtureRequest
    from conftest import (
        KeyPair,
    )
    from docker.client import DockerClient


CEREMONY_MASTER_NODE_KEYS = conftest.KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97')
VALIDATOR_A_KEYS = conftest.KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821')
VALIDATOR_B_KEYS = conftest.KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e')


@pytest.mark.xfail
def test_successful_genesis_ceremony(docker_client_session: "DockerClient") -> None:
    assert False
