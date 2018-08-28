import logging
import re
import tempfile
import random
import tools.resources as resources
from tools.util import log_box

from multiprocessing import Queue, Process
from queue import Empty

default_image = "rchain-integration-testing:latest"

rnode_binary='/opt/docker/bin/rnode'
rnode_directory = "/var/lib/rnode"
rnode_deploy_dir = f"{rnode_directory}/deploy"
rnode_bonds_file = f'{rnode_directory}/genesis/bonds.txt'
rnode_certificate = f'{rnode_directory}/node.certificate.pem'
rnode_key = f'{rnode_directory}/node.key.pem'

class InterruptedException(Exception):
    pass

class Node:
    def __init__(self, container, deploy_dir, docker_client, timeout):
        self.container = container
        self.local_deploy_dir = deploy_dir
        self.remote_deploy_dir = rnode_deploy_dir
        self.name = container.name
        self.docker_client = docker_client
        self.timeout = timeout

    def logs(self):
        return self.container.logs().decode('utf-8')

    def get_rnode_address(self):
        log_content = self.logs()
        m = re.search("Listening for traffic on (rnode://.*:\d+)\.$", log_content, re.MULTILINE | re.DOTALL)
        address = m[1]

        logging.info(f"Bootstrap address: `{address}`")
        return address

    def cleanup(self):
        with log_box(logging.info, f"Logs for node {self.container.name}:"):
            logs = self.logs().splitlines()
            for log_line in logs:
                logging.info(f"{self.container.name}: {log_line}")

        logging.info(f"Remove container {self.container.name}")
        self.container.remove(force=True, v=True)

    def deploy(self, contract):
        cmd = f'{rnode_binary} deploy --from "0x1" --phlo-limit 0 --phlo-price 0 --nonce 0 {rnode_deploy_dir}/{contract}'
        return self.exec_run(cmd)

    def propose(self):
        return self.exec_run(f'{rnode_binary} propose')

    def show_blocks(self):
        return self.exec_run(f'{rnode_binary} show-blocks')

    def exec_run(self, cmd):
        queue = Queue(1)

        def execution():
            r = self.container.exec_run(cmd)
            queue.put((r.exit_code, r.output.decode('utf-8')))

        process = Process(target=execution)

        logging.info(f"Execute '{cmd}' in a separate process")

        process.start()

        try:
            logging.debug(f"Wait for result for {self.timeout}s")
            result = queue.get(self.timeout)
            logging.debug("Returning '{result}'")
            return result
        except Empty:
            process.terminate()
            process.join()
            raise Exception(f"The command '{cmd}' hasn't finished execution after {self.timeout}s")

    __timestamp_rx = "\d\d:\d\d:\d\d\.\d\d\d"
    __log_message_rx = re.compile(f"^{__timestamp_rx} (.*?)(?={__timestamp_rx})", re.MULTILINE | re.DOTALL)


    def log_lines(self):
        log_content = self.logs()
        return Node.__log_message_rx.split(log_content)


def __read_validator_keys():
    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    f=open(resources.file_path('pregenerated-validator-private-public-key-pairs.txt'))
    lines=f.readlines()
    random.shuffle(lines)
    return [line.split() for line in lines]

validator_keys = __read_validator_keys()

def __create_node_container(docker_client, image, name, network, command, rnode_timeout, extra_volumes, memory, cpuset_cpus):
    deploy_dir = tempfile.mkdtemp(dir="/tmp", prefix="rchain-integration-test")

    bonds_file = resources.file_path("test-bonds.txt")

    container  = docker_client.containers.run( image,
                                               name=name,
                                               user='root',
                                               detach=True,
                                               cpuset_cpus=cpuset_cpus,
                                               mem_limit=memory,
                                               network=network,
                                               volumes=[
                                                           f"{bonds_file}:{rnode_bonds_file}",
                                                           f"{deploy_dir}:{rnode_deploy_dir}"
                                                       ] + extra_volumes,
                                               command=command,
                                               hostname=name)
    return Node(container, deploy_dir, docker_client, rnode_timeout)

def create_bootstrap_node(docker_client, network, rnode_timeout, image=default_image, memory="1024m", cpuset_cpus="0"):
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
        f"{cert_file}:{rnode_certificate}",
        f"{key_file}:{rnode_key}"
    ]

    logging.info(f"Starting bootstrap node {name}\ncommand:`{command}`")

    return __create_node_container(docker_client, image, name, network, command, rnode_timeout, volumes, memory, cpuset_cpus)

def create_peer_nodes(docker_client, n, bootstrap, network, rnode_timeout, image=default_image, memory="1024m", cpuset_cpus="0"):
    """
    Create peer nodes
    """
    bootstrap_address = bootstrap.get_rnode_address()

    logging.info(f"Create {n} peer nodes to connect to bootstrap {bootstrap_address}.")

    def create_peer(i, private_key, public_key):
        name = f"peer{i}.{network}"
        command = f"run --bootstrap {bootstrap_address} --validator-private-key {private_key} --validator-public-key {public_key} --host {name}"

        logging.info(f"Starting peer node {name} with command: `{command}`")
        return __create_node_container(docker_client, image, name, network, command, rnode_timeout, [], memory, cpuset_cpus)

    return [ create_peer(i, sk, pk)
             for i, (sk, pk) in enumerate(validator_keys[1:n+1])]
