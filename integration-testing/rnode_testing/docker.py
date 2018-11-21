import logging
from contextlib import contextmanager
import rnode_testing.random


def run_cmd(docker_container, cmd):
    logging.info("{name}: Execute <{cmd}>".format(name=docker_container.name, cmd=cmd))
    r = docker_container.exec_run(['sh', '-c', cmd])
    output = r.output.decode('utf-8')

    logging.info("{name}: Finish <{cmd}>. Exit Code: {exit_code}".format(name=docker_container.name, cmd=cmd, exit_code=r.exit_code))
    return (r.exit_code, output)


def list_containers(docker_client, network):
    return docker_client.containers.list(all=True, filters={"name": ".{network}".format(network=network)})


@contextmanager
def docker_network(docker_client):
    network_name = "rchain-{random_string}".format(random_string=rnode_testing.random.random_string(5).lower())

    docker_client.networks.create(network_name, driver="bridge")

    try:
        yield network_name
    finally:
        for network in docker_client.networks.list():
            if network_name == network.name:
                logging.info("Removing docker network {name}".format(name=network.name))
                network.remove()
