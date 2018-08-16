import logging
import re
import docker
import random
import tools.resources as resources


rnode_cmd = '/opt/docker/bin/rnode'
rnode_directory = "/var/lib/rnode"


def get_rnode_address(container):
    log_content = container.logs().decode('utf-8')
    m = re.search("Listening for traffic on (rnode://.*:\d+)\.$", log_content, re.MULTILINE | re.DOTALL)
    address = m[1]

    logging.info(f"Bootstrap address: `{address}`")
    return address

def __read_validator_keys():
    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    f=open(resources.file_path('pregenerated-validator-private-public-key-pairs.txt'))
    lines=f.readlines()
    random.shuffle(lines)
    return [line.split() for line in lines]

validator_keys = __read_validator_keys()

def __create_node_container(docker_client, image, name, network, command, extra_volumes, memory, cpuset_cpus):
    bonds_file = resources.file_path("test-bonds.txt")

    container_bonds_file = f'{rnode_directory}/genesis/bonds.txt'

    volume = docker_client.volumes.create()
    return docker_client.containers.run( image,
                                         name=name,
                                         user='root',
                                         detach=True,
                                         cpuset_cpus=cpuset_cpus,
                                         mem_limit=memory,
                                         network=network,
                                         volumes=[
                                                     f"{volume.name}:{rnode_directory}",
                                                     f"{bonds_file}:{container_bonds_file}"
                                                 ] + extra_volumes,
                                         command=command,
                                         hostname=name)

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

def create_peer_nodes(docker_client, n, bootstrap_address, network, image="test-image:latest", memory="1024m", cpuset_cpus="0"):
    """
    Create peer nodes
    """

    logging.info(f"Create {n} peer nodes to connect to bootstrap {bootstrap_address}.")

    def create_peer(i, private_key, public_key):
        name = f"peer{i}.{network}"
        command = f"run --bootstrap {bootstrap_address} --validator-private-key {private_key} --validator-public-key {public_key} --host {name}"

        logging.info(f"Starting peer node {name} with command: `{command}`")
        return __create_node_container(docker_client, image, name, network, command, [], memory, cpuset_cpus)

    return [ create_peer(i, sk, pk)
             for i, (sk, pk) in enumerate(validator_keys[1:n+1])]

