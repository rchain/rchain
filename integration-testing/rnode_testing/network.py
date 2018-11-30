import logging
import pytest
import contextlib

from rnode_testing.rnode import create_peer_nodes

from typing import List, TYPE_CHECKING, Callable, Any, Optional, Generator

if TYPE_CHECKING:
    from conftest import TestConfig, ValidatorsData
    from docker.client import DockerClient
    from rnode_testing.rnode import Node

def make_wrapper(fn: Callable, fixtures: Any):
    parameter_names = inspect.signature(fn).parameters.keys()
    parameter_list = ",".join(p for p in parameter_names if p != 'request')
    parameter_name_list = ",".join("('{p}', {p})".format(p=p) for p in parameter_names)
    namespace = {"fn": fn, "fixtures": fixtures}

    wrapper_code = """
def {fn_name}(request, {parameter_list}):
    # import logging
    # logging.info("fixtures:" + str(fixtures))
    def get_value(p_name, p_value):
        if p_name in fixtures:
            v = request.getfixturevalue(p_value.__name__)
            # logging.info("Get_value from fixtures: " + p_name + " : " + str(v))
            return v
        else:
            # logging.info("Get_value returns the object: " + p_name + " : " + str(p_value))
            return p_value
    param_values = [get_value(p_name, p_value) for p_name, p_value in [{parameter_name_list}]]
    return fn(*param_values)
""".format(fn_name=fn.__name__, parameter_list=parameter_list, parameter_name_list=parameter_name_list)

    exec(wrapper_code, locals(), namespace)
    return namespace[fn.__name__]


class parametrize:
    @staticmethod
    def fixed(arg_names: str, arg_values: List):
        def decorator(fn: Callable):
            fixtures = arg_names.split(',')
            wrapper = make_wrapper(fn, fixtures)
            return pytest.mark.parametrize(arg_names, arg_values)(wrapper)
        return decorator

    @staticmethod
    def cartesian(**kwargs):
        def decorator(fn: Callable) -> Callable:
            fixtures = kwargs.keys()

            result_fn = make_wrapper(fn, fixtures)
            for arg in kwargs.keys():
                result_fn = pytest.mark.parametrize(arg, kwargs[arg])(result_fn)
            return result_fn
        return decorator


class Network:
    def __init__(self, network: str, bootstrap: "Node", peers: List["Node"]) -> None:
        self.network = network
        self.bootstrap = bootstrap
        self.peers = peers
        self.nodes = [bootstrap] + peers


@contextlib.contextmanager
def start_network(config: "TestConfig", docker: "DockerClient", bootstrap: "Node", validators_data: "ValidatorsData", allowed_peers: Optional[List[str]] = None) -> Generator[RChain, None, None]:
    logging.debug("Docker network = {}".format(bootstrap.network))

    peers = create_peer_nodes(
        docker_client=docker,
        bootstrap=bootstrap,
        network=bootstrap.network,
        bonds_file=validators_data.bonds_file,
        key_pairs=validators_data.peers_keys,
        rnode_timeout=config.rnode_timeout,
        allowed_peers=allowed_peers,
    )

    try:
        yield Network(network=bootstrap.network, bootstrap=bootstrap, peers=peers)
    finally:
        for peer in peers:
            peer.cleanup()
