import logging
from contextlib import contextmanager
import rnode_testing.random


def run_cmd(docker_container, cmd):
    logging.info(f"{docker_container.name}: Execute <{cmd}>")
    r = docker_container.exec_run(['sh', '-c', cmd])
    output = r.output.decode('utf-8')

    logging.info(f"{docker_container.name}: Finish <{cmd}>. Exit Code: {r.exit_code}")
    return (r.exit_code, output)


def list_containers(docker_client, network):
    return docker_client.containers.list(all=True, filters={"name": f".{network}"})


@contextmanager
def docker_network(docker_client):
    network_name = f"rchain-{rnode_testing.random.random_string(5).lower()}"

    docker_client.networks.create(network_name, driver="bridge")

    try:
        yield network_name

    finally:
        for network in docker_client.networks.list():
            if network_name == network.name:
                logging.info(f"Removing docker network {network.name}")
                network.remove()
