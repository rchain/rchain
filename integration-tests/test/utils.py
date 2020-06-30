import os
import platform
import socket
import subprocess
from collections import defaultdict
from contextlib import contextmanager
from typing import Dict, Set, Generator

from docker import DockerClient

ISLINUX = platform.system() == 'Linux'


def parse_mvdag_str(mvdag_output: str) -> Dict[str, Set[str]]:
    dag_dict: Dict[str, Set[str]] = defaultdict(set)

    lines = mvdag_output.splitlines()
    for line in lines:
        parent_hash, child_hash = line.split(' ')
        dag_dict[parent_hash].add(child_hash)
    return dag_dict

def get_current_container_id() -> str:
    hostname = subprocess.run(['hostname'], check=True, stdout=subprocess.PIPE)
    return hostname.stdout.decode('utf8').strip("\n")

@contextmanager
def get_node_ip_of_network(docker_client: DockerClient, network_name: str) -> Generator[str, None, None]:
    """
    In a drone mode, the current pytest process is within a docker container.In order
    to create the connection with the node, we have to attach the current pytest container
    to the node network.

    In a local linux mode, the current pytest process is a normal system process which is
    running in the host machine. So as for the container, the gateway ip is the host
    machine ip.

    WARNING: For Windows and MacOS, you can not connect to the container from host
    without any other configuration. For more info, see https://github.com/docker/for-mac/issues/2670.
    """
    if os.environ.get("DRONE") == 'true':
        current_container_id = get_current_container_id()
        current_container = docker_client.containers.get(current_container_id)
        network = docker_client.networks.get(network_name)
        try:
            network.connect(current_container)
            network.reload()
            container_network_config = network.attrs['Containers'][current_container.id]
            ip, _ = container_network_config['IPv4Address'].split('/')
            yield ip
        finally:
            network.disconnect(current_container)
    else:
        network_attr = docker_client.networks.get(network_name).attrs
        ipam_attr = network_attr.get("IPAM")
        assert ipam_attr is not None
        ip = ipam_attr['Config'][0]['Gateway']
        yield ip


def get_free_tcp_port() -> int:
    tcp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        tcp.bind(('', 0))
        _, port = tcp.getsockname()
    finally:
        tcp.close()
    return port
