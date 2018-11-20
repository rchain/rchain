import os
import logging
import re
from contextlib import contextmanager
from rnode_testing.docker import docker_network
import rnode_testing.resources as resources
from rnode_testing.util import log_box, make_tempfile, make_tempdir
from rnode_testing.wait import wait_for, node_started
import shlex

from multiprocessing import Queue, Process
from queue import Empty

DEFAULT_IMAGE = "rchain-integration-testing:latest"

rnode_binary = '/opt/docker/bin/rnode'
rnode_directory = "/var/lib/rnode"
rnode_deploy_dir = f"{rnode_directory}/deploy"
rnode_bonds_file = f'{rnode_directory}/genesis/bonds.txt'
rnode_certificate = f'{rnode_directory}/node.certificate.pem'
rnode_key = f'{rnode_directory}/node.key.pem'


class InterruptedException(Exception):
    pass


class NonZeroExitCodeError(Exception):
    def __init__(self, command, exit_code, output):
        self.command = command
        self.exit_code = exit_code
        self.output = output


class TimeoutError(Exception):
    def __init__(self, command, timeout):
        self.command = command
        self.timeout = timeout


def make_container_logs_path(container_name):
    ci_logs_dir = os.environ.get('CI_LOGS_DIR')
    dir = 'logs' if ci_logs_dir is None else ci_logs_dir
    return os.path.join(dir, "{}.log".format(container_name))


class Node:
    def __init__(self, container, deploy_dir, docker_client, timeout, network):
        self.container = container
        self.local_deploy_dir = deploy_dir
        self.remote_deploy_dir = rnode_deploy_dir
        self.name = container.name
        self.docker_client = docker_client
        self.timeout = timeout
        self.network = network

    def logs(self):
        return self.container.logs().decode('utf-8')

    def get_rnode_address(self):
        log_content = self.logs()
        m = re.search(f"Listening for traffic on (rnode://.+@{self.container.name}\\?protocol=\\d+&discovery=\\d+)\\.$", log_content, re.MULTILINE | re.DOTALL)
        address = m[1]

        logging.info(f"Bootstrap address: `{address}`")
        return address

    def get_metrics(self):
        cmd = f'curl -s http://localhost:40403/metrics'

        return self.exec_run(cmd=cmd)

    def cleanup(self):
        log_file_path = make_container_logs_path(self.container.name)

        with open(log_file_path, "w") as f:
            f.write(self.logs())

        logging.info(f"Remove container {self.container.name}. Logs have been written to {log_file_path}")

        self.container.remove(force=True, v=True)

    def deploy_contract(self, contract):
        cmd = f'{rnode_binary} deploy --from "0x1" --phlo-limit 1000000 --phlo-price 1 --nonce 0 {rnode_deploy_dir}/{contract}'
        return self.exec_run(cmd)

    def propose_contract(self):
        return self.exec_run(f'{rnode_binary} propose')

    def show_blocks(self):
        return self.exec_run(f'{rnode_binary} show-blocks')

    def get_blocks_count(self):
        output = self.call_rnode('show-blocks', stderr=False).strip()
        spam = 'count: '
        assert output.startswith(spam)
        blocks_count = int(output[len(spam):])
        return blocks_count

    def exec_run(self, cmd, stderr=True):
        queue = Queue(1)

        def execution():
            r = self.container.exec_run(cmd, stderr=stderr)
            queue.put((r.exit_code, r.output.decode('utf-8')))

        process = Process(target=execution)

        logging.info(f"{self.name}: Execute '{cmd}'. Timeout: {self.timeout}s")

        process.start()

        try:
            exit_code, output = queue.get(self.timeout)
            logging.info(f"Returning: {exit_code}, '{output}'")
            return exit_code, output
        except Empty:
            process.terminate()
            process.join()
            raise TimeoutError(cmd, self.timeout)

    def shell_out(self, *cmd, stderr=True):
        exit_code, output = self.exec_run(cmd, stderr=stderr)
        if exit_code != 0:
            raise NonZeroExitCodeError(command=cmd, exit_code=exit_code, output=output)
        return output

    def call_rnode(self, *node_args, stderr=True):
        return self.shell_out(rnode_binary, *node_args, stderr=stderr)

    def eval(self, rho_file_path):
        return self.call_rnode('eval', rho_file_path)

    def deploy(self, rho_file_path):
        return self.call_rnode('deploy', '--from=0x1', '--phlo-limit=1000000', '--phlo-price=1', '--nonce=0', rho_file_path)

    def deploy_string(self, rholang_code):
        quoted_rholang = shlex.quote(rholang_code)
        return self.shell_out('sh', '-c', 'echo {quoted_rholang} >/tmp/deploy_string.rho && {rnode_binary} deploy --phlo-limit=10000000000 --phlo-price=1 /tmp/deploy_string.rho'.format(
            rnode_binary=rnode_binary,
            quoted_rholang=quoted_rholang,
        ))

    def propose(self):
        return self.call_rnode('propose')

    def repl(self, rholang_code, stderr=False):
        quoted_rholang_code = shlex.quote(rholang_code)
        return self.shell_out('sh', '-c', f'echo {quoted_rholang_code} | {rnode_binary} repl', stderr=stderr)

    def generate_faucet_bonding_deploys(self, bond_amount, private_key, public_key):
        return self.call_rnode('generateFaucetBondingDeploys',
            '--amount={}'.format(bond_amount),
            '--private-key={}'.format(private_key),
            '--public-key={}'.format(public_key),
            '--sig-algorithm=ed25519',
        )

    def cat_forward_file(self, public_key):
        return self.shell_out('cat', '/opt/docker/forward_{}.rho'.format(public_key))

    def cat_bond_file(self, public_key):
        return self.shell_out('cat', '/opt/docker/bond_{}.rho'.format(public_key))

    __timestamp_rx = "\\d\\d:\\d\\d:\\d\\d\\.\\d\\d\\d"
    __log_message_rx = re.compile(f"^{__timestamp_rx} (.*?)(?={__timestamp_rx})", re.MULTILINE | re.DOTALL)

    def log_lines(self):
        log_content = self.logs()
        return Node.__log_message_rx.split(log_content)


def make_container_command(container_command, container_command_options):
    opts = ['{} {}'.format(option, argument) for option, argument in container_command_options.items()]
    result = '{} {}'.format(container_command, ' '.join(opts))
    return result


def create_node_container(
    *,
    docker_client,
    name, network,
    bonds_file,
    container_command,
    container_command_options,
    rnode_timeout,
    extra_volumes,
    allowed_peers,
    cpuset_cpus,
    image=DEFAULT_IMAGE,
    mem_limit=None,
):
    assert isinstance(name, str)
    assert '_' not in name, 'Underscore is not allowed in host name'
    deploy_dir = make_tempdir("rchain-integration-test")

    hosts_allow_file_content = \
        "ALL:ALL" if allowed_peers is None else "\n".join(f"ALL: {peer}" for peer in allowed_peers)

    hosts_allow_file = make_tempfile(f"hosts-allow-{name}", hosts_allow_file_content)
    hosts_deny_file = make_tempfile(f"hosts-deny-{name}", "ALL: ALL")

    command = make_container_command(container_command, container_command_options)

    env = {}
    java_options = os.environ.get('_JAVA_OPTIONS')
    if java_options is not None:
        env['_JAVA_OPTIONS'] = java_options
    logging.info('Using _JAVA_OPTIONS: {}'.format(java_options))

    volumes = [
        f"{hosts_allow_file}:/etc/hosts.allow",
        f"{hosts_deny_file}:/etc/hosts.deny",
        f"{bonds_file}:{rnode_bonds_file}",
        f"{deploy_dir}:{rnode_deploy_dir}",
    ]

    container = docker_client.containers.run(
        image,
        name=name,
        user='root',
        detach=True,
        cpuset_cpus=cpuset_cpus,
        mem_limit=mem_limit,
        network=network,
        volumes=volumes + extra_volumes,
        command=command,
        hostname=name,
        environment=env,
    )

    return Node(container, deploy_dir, docker_client, rnode_timeout, network)


def create_bootstrap_node(
    *,
    docker_client,
    network,
    bonds_file,
    key_pair,
    rnode_timeout,
    allowed_peers=None,
    image=DEFAULT_IMAGE,
    cpuset_cpus="0",
    mem_limit=None,
):
    key_file = resources.get_resource_path("bootstrap_certificate/node.key.pem")
    cert_file = resources.get_resource_path("bootstrap_certificate/node.certificate.pem")

    logging.info(f"Using key_file={key_file} and cert_file={cert_file}")

    name = f"bootstrap.{network}"
    container_command_options = {
        "--port":                   40400,
        "--standalone":             "",
        "--validator-private-key":  key_pair.private_key,
        "--validator-public-key":   key_pair.public_key,
        "--host":                   name,
    }

    volumes = [
        f"{cert_file}:{rnode_certificate}",
        f"{key_file}:{rnode_key}"
    ]

    container = create_node_container(
        docker_client=docker_client,
        name=name,
        network=network,
        bonds_file=bonds_file,
        container_command='run',
        container_command_options=container_command_options,
        rnode_timeout=rnode_timeout,
        extra_volumes=volumes,
        allowed_peers=allowed_peers,
        mem_limit=mem_limit if mem_limit is not None else '4G',
        cpuset_cpus=cpuset_cpus,
    )
    return container


def make_peer_name(network, i):
    return f"peer{i}.{network}"


def create_peer(
    *,
    docker_client,
    network,
    name,
    bonds_file,
    rnode_timeout,
    bootstrap,
    key_pair,
    allowed_peers=None,
    image=DEFAULT_IMAGE,
    cpuset_cpus="0",
    mem_limit=None,
):
    assert isinstance(name, str)
    assert '_' not in name, 'Underscore is not allowed in host name'
    name = make_peer_name(network, name)

    bootstrap_address = bootstrap.get_rnode_address()

    container_command_options = {
        "--bootstrap":              bootstrap_address,
        "--validator-private-key":  key_pair.private_key,
        "--validator-public-key":   key_pair.public_key,
        "--host":                   name,
    }

    container = create_node_container(
        docker_client=docker_client,
        name=name,
        network=network,
        bonds_file=bonds_file,
        container_command='run',
        container_command_options=container_command_options,
        rnode_timeout=rnode_timeout,
        extra_volumes=[],
        allowed_peers=allowed_peers,
        mem_limit=mem_limit if not None else '4G',
        cpuset_cpus=cpuset_cpus,
    )
    return container


def create_peer_nodes(docker_client,
                      bootstrap,
                      network,
                      bonds_file,
                      key_pairs,
                      rnode_timeout,
                      allowed_peers=None,
                      image=DEFAULT_IMAGE,
                      mem_limit=None,
                      cpuset_cpus="0"):
    assert len(set(key_pairs)) == len(key_pairs), "There shouldn't be any duplicates in the key pairs"

    if allowed_peers is None:
        allowed_peers = [bootstrap.name] + [make_peer_name(network, i) for i in range(0, len(key_pairs))]

    result = []
    try:
        for i, key_pair in enumerate(key_pairs):
            peer_node = create_peer(
                docker_client=docker_client,
                network=network,
                name=str(i),
                bonds_file=bonds_file,
                rnode_timeout=rnode_timeout,
                bootstrap=bootstrap,
                key_pair=key_pair,
                allowed_peers=allowed_peers,
                image=image,
                mem_limit=mem_limit if mem_limit is not None else '4G',
                cpuset_cpus=cpuset_cpus,
            )
            result.append(peer_node)
    except:
        for node in result:
            node.cleanup()
        raise
    return result


@contextmanager
def create_bootstrap(docker, docker_network, timeout, validators_data):
    node = create_bootstrap_node(
        docker_client=docker,
        network=docker_network,
        bonds_file=validators_data.bonds_file,
        key_pair=validators_data.bootstrap_keys,
        rnode_timeout=timeout,
    )
    try:
        yield node
    finally:
        node.cleanup()


@contextmanager
def start_bootstrap(docker_client, node_start_timeout, node_cmd_timeout, validators_data):
    with docker_network(docker_client) as network:
        with create_bootstrap(docker_client, network, node_cmd_timeout, validators_data) as node:
            wait_for(node_started(node), node_start_timeout, "Bootstrap node didn't start correctly")
            yield node
