import logging
import re
import tempfile
import random
import tools.resources as resources
import collections

class Node:
    def __init__(self, container, deploy_dir):
        self.container = container
        self.deploy_dir = deploy_dir
        self.name = container.name

    def logs(self):
        return self.container.logs().decode('utf-8')

    def get_rnode_address(self):
        log_content = self.logs()
        m = re.search("Listening for traffic on (rnode://.*:\d+)\.$", log_content, re.MULTILINE | re.DOTALL)
        address = m[1]

        logging.info(f"Bootstrap address: `{address}`")
        return address

    def cleanup(self):
        logging.info("=" * 100)

        logging.info(f"Docker container logs for {self.container.name}:")

        logging.info("=" * 100)

        logs = self.logs().splitlines()
        for log_line in logs:
            logging.info(f"{self.container.name}: {log_line}")

        logging.info("=" * 100)

        logging.info(f"Remove container {self.container.name}")
        self.container.remove(force=True, v=True)

rnode_cmd = '/opt/docker/bin/rnode'
rnode_directory = "/var/lib/rnode"

def __read_validator_keys():
    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    f=open(resources.file_path('pregenerated-validator-private-public-key-pairs.txt'))
    lines=f.readlines()
    random.shuffle(lines)
    return [line.split() for line in lines]

validator_keys = __read_validator_keys()

def __create_node_container(docker_client, image, name, network, command, extra_volumes, memory, cpuset_cpus):
    deploy_dir = tempfile.mkdtemp(dir="/tmp", prefix="rchain-integration-test")
    container_deploy_dir = f"{rnode_directory}/deploy"

    bonds_file = resources.file_path("test-bonds.txt")
    container_bonds_file = f'{rnode_directory}/genesis/bonds.txt'

    container  = docker_client.containers.run( image,
                                               name=name,
                                               user='root',
                                               detach=True,
                                               cpuset_cpus=cpuset_cpus,
                                               mem_limit=memory,
                                               network=network,
                                               volumes=[
                                                           f"{bonds_file}:{container_bonds_file}",
                                                           f"{deploy_dir}:{container_deploy_dir}"
                                                       ] + extra_volumes,
                                               command=command,
                                               hostname=name)
    return Node(container, deploy_dir)

def create_bootstrap_node(docker_client, network, image="test-image:latest", memory="1024m", cpuset_cpus="0"):
    """
    Create bootstrap node.
    """

    validator_private_key, validator_public_key = validator_keys[0]

    key_file = resources.file_path("bootstrap_certificate/node.key.pem")
    cert_file = resources.file_path("bootstrap_certificate/node.certificate.pem")

    logging.info(f"Using key_file={key_file} and cert_file={cert_file}")

    name = f"bootstrap.{network}"
    command = f"run --port 40400 --standalone --validator-private-key {validator_private_key} --validator-public-key {validator_public_key} --host {name}"

    volumes = [
        f"{cert_file}:{rnode_directory}/node.certificate.pem",
        f"{key_file}:{rnode_directory}/node.key.pem"
    ]

    logging.info(f"Starting bootstrap node {name}\ncommand:`{command}`")

    return __create_node_container(docker_client, image, name, network, command, volumes, memory, cpuset_cpus)

def create_peer_nodes(docker_client, n, bootstrap, network, image="test-image:latest", memory="1024m", cpuset_cpus="0"):
    """
    Create peer nodes
    """
    bootstrap_address = bootstrap.get_rnode_address()

    logging.info(f"Create {n} peer nodes to connect to bootstrap {bootstrap_address}.")

    def create_peer(i, private_key, public_key):
        name = f"peer{i}.{network}"
        command = f"run --bootstrap {bootstrap_address} --validator-private-key {private_key} --validator-public-key {public_key} --host {name}"

        logging.info(f"Starting peer node {name} with command: `{command}`")
        return __create_node_container(docker_client, image, name, network, command, [], memory, cpuset_cpus)

    return [ create_peer(i, sk, pk)
             for i, (sk, pk) in enumerate(validator_keys[1:n+1])]
