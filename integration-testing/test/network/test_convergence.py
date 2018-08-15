
import logging
from fixtures import *
from tools.rnode import create_bootstrap_node, get_rnode_address, create_peer_nodes
from tools.wait import wait_for, contains, container_logs, network_converged

def test_convergence(config, docker_network):
    """
    This test represents an integration test that starts a network and checks if it converges after the de.
    """
    logging.debug(f"Docker network = {docker_network}")

    bootstrap = create_bootstrap_node(docker_network)

    assert wait_for( contains( container_logs(bootstrap),
                               "coop.rchain.node.NodeRuntime - Starting stand-alone node."),
                     config.bootstrap_startup_timeout),\
        "Bootstrap node didn't start correctly"

    bootstrap_address  = get_rnode_address(bootstrap)

    peers = create_peer_nodes(config.peer_count, bootstrap_address, docker_network)

    assert wait_for( network_converged(bootstrap, config.peer_count),
                     config.network_converge_timeout, 10),\
        "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect."
