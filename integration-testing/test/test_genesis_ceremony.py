import contextlib
import pytest
import conftest
from rnode_testing.rnode import start_bootstrap, create_peer
from rnode_testing.wait import sent_unapproved_block

from typing import Iterator, List, TYPE_CHECKING
if TYPE_CHECKING:
    from _pytest.fixtures import FixtureRequest
    from conftest import (
        KeyPair,
        System,
        ValidatorsData,
    )
    from docker.client import DockerClient


CEREMONY_MASTER_NODE_KEYS = conftest.KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97')
VALIDATOR_A_KEYS = conftest.KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821')
VALIDATOR_B_KEYS = conftest.KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e')


@contextlib.contextmanager
def validators_config(validator_keys: List["KeyPair"]) -> Iterator["ValidatorsData"]:
    with conftest.temporary_bonds_file(validator_keys) as f:
        yield conftest.ValidatorsData(bonds_file=f, bootstrap_keys=CEREMONY_MASTER_NODE_KEYS, peers_keys=validator_keys)


@contextlib.contextmanager
def custom_system(request: "FixtureRequest", docker_client_session: "DockerClient", validator_keys: List["KeyPair"]) -> Iterator["System"]:
    with validators_config(validator_keys) as config:
        test_config = conftest.make_test_config(request)
        yield conftest.System(test_config, docker_client_session, config)


@pytest.mark.xfail
def test_successful_genesis_ceremony(request: "FixtureRequest", docker_client_session: "DockerClient") -> None:
    cli_options = {
        '--required-sigs':  2,
        '--duration':       '5 min',
        '--interval':       '10 sec',
    }
    with custom_system(request, docker_client_session, validator_keys=[VALIDATOR_A_KEYS, VALIDATOR_B_KEYS]) as system:
        with start_bootstrap(
            system.docker,
            system.config.node_startup_timeout,
            system.config.rnode_timeout,
            system.validators_data,
            container_name='ceremony-master',
            cli_options=cli_options,
            mount_dir=system.config.mount_dir,
        ) as bootstrap:
            # wait_for(sent_unapproved_block, ...
            assert False
