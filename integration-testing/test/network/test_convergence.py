
import logging
from fixtures import docker_network
from tools.rnode import *
from tools.wait import *

def test_convergence(docker_network):
    """
    This test represents an integration test that starts a network and checks if it converges after the de.
    """

    peer_count = 2
    bootstrap_startup_timeout = 30
    network_converge_timeout = 20#0
    network_converge_iteration = 5

    logging.debug(f"Docker network = {docker_network}")

    bootstrap = create_bootstrap_node(docker_network)

    assert wait_for( contain(container_logs(bootstrap), "coop.rchain.node.NodeRuntime - Starting stand-alone node."), bootstrap_startup_timeout),\
        "Bootstrap node didn't start correctly"

    bootstrap_address  = get_rnode_address(bootstrap)

    peers = create_peer_nodes(peer_count, bootstrap_address, docker_network)

    assert wait_for( network_converged(bootstrap, peer_count), network_converge_timeout, network_converge_iteration),\
        "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect."
