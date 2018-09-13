import logging
import re
import tempfile
import tools.resources as resources
from tools.util import log_box, make_tempfile, make_tempdir

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

        logging.info(f"{self.name}: Execute '{cmd}'. Timeout: {self.timeout}s")

        process.start()

        try:
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

def __create_node_container(docker_client, image, name, network, bonds_file, command, rnode_timeout, extra_volumes, allowed_peers, memory, cpuset_cpus):
    deploy_dir = make_tempdir("rchain-integration-test")

    hosts_allow_file_content = \
        "ALL:ALL" if allowed_peers == None else "\n".join(f"ALL: {peer}" for peer in allowed_peers)

    hosts_allow_file = make_tempfile(f"hosts-allow-{name}", hosts_allow_file_content)
    hosts_deny_file = make_tempfile(f"hosts-deny-{name}", "ALL: ALL")

    container  = docker_client.containers.run( image,
                                               name=name,
                                               user='root',
                                               detach=True,
                                               cpuset_cpus=cpuset_cpus,
                                               mem_limit=memory,
                                               network=network,
                                               volumes=[
                                                           f"{hosts_allow_file}:/etc/hosts.allow",
                                                           f"{hosts_deny_file}:/etc/hosts.deny",
                                                           f"{bonds_file}:{rnode_bonds_file}",
                                                           f"{deploy_dir}:{rnode_deploy_dir}"
                                                       ] + extra_volumes,
                                               command=command,
                                               hostname=name)
    return Node(container, deploy_dir, docker_client, rnode_timeout)

def create_bootstrap_node(docker_client, network, bonds_file, key_pair, rnode_timeout, allowed_peers = None, image=default_image, memory="1024m", cpuset_cpus="0"):
    """
    Create bootstrap node.
    """

    key_file = resources.file_path("bootstrap_certificate/node.key.pem")
    cert_file = resources.file_path("bootstrap_certificate/node.certificate.pem")

    logging.info(f"Using key_file={key_file} and cert_file={cert_file}")

    name = f"bootstrap.{network}"
    command = f"run --port 40400 --standalone --validator-private-key {key_pair.private_key} --validator-public-key {key_pair.public_key} --host {name}"

    volumes = [
        f"{cert_file}:{rnode_certificate}",
        f"{key_file}:{rnode_key}"
    ]

    logging.info(f"Starting bootstrap node {name}\ncommand:`{command}`")

    return __create_node_container(docker_client, image, name, network, bonds_file, command, rnode_timeout, volumes, allowed_peers, memory, cpuset_cpus)

def create_peer_nodes(docker_client, bootstrap, network, bonds_file, key_pairs, rnode_timeout, allowed_peers = None, image=default_image, memory="1024m", cpuset_cpus="0"):
    """
    Create peer nodes
    """
    assert len(set(key_pairs)) == len(key_pairs), "There shouldn't be any duplicates in the key pairs"

    def peer_name(i): return f"peer{i}.{network}"

    if allowed_peers == None:
        allowed_peers = [bootstrap.name] + [peer_name(i) for i in range(0, len(key_pairs))]

    bootstrap_address = bootstrap.get_rnode_address()

    logging.info(f"Create {len(key_pairs)} peer nodes to connect to bootstrap {bootstrap_address}.")

    def create_peer(i, key_pair):
        name = peer_name(i)
        command = f"run --bootstrap {bootstrap_address} --validator-private-key {key_pair.private_key} --validator-public-key {key_pair.public_key} --host {name}"

        logging.info(f"Starting peer node {name} with command: `{command}`")

        return __create_node_container(docker_client, image, name, network, bonds_file, command, rnode_timeout, [], allowed_peers, memory, cpuset_cpus)

    return [ create_peer(i, key_pair)
             for i, key_pair in enumerate(key_pairs)]
