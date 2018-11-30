import re
import os
import shlex
import logging
import threading
import contextlib

from rnode_testing.common import (
    random_string,
    make_tempfile,
    make_tempdir,
)
from rnode_testing.wait import (
    wait_for_node_started,
)

from multiprocessing import Queue, Process
from queue import Empty

from typing import Dict, List, Tuple, Union, TYPE_CHECKING, Optional, Generator

if TYPE_CHECKING:
    from conftest import ValidatorsData, KeyPair
    from docker.client import DockerClient
    from docker.models.containers import Container
    from logging import Logger
    from threading import Event

DEFAULT_IMAGE = os.environ.get(
        "DEFAULT_IMAGE",
        "rchain-integration-testing:latest")

rnode_binary = '/opt/docker/bin/rnode'
rnode_directory = "/var/lib/rnode"
rnode_deploy_dir = "{}/deploy".format(rnode_directory)
rnode_bonds_file = '{}/genesis/bonds.txt'.format(rnode_directory)
rnode_certificate = '{}/node.certificate.pem'.format(rnode_directory)
rnode_key = '{}/node.key.pem'.format(rnode_directory)


class InterruptedException(Exception):
    pass


class RNodeAddressNotFoundError(Exception):
    pass


class NonZeroExitCodeError(Exception):
    def __init__(self, command: Tuple[Union[int, str], ...], exit_code: int, output: str):
        self.command = command
        self.exit_code = exit_code
        self.output = output

    def __repr__(self) -> str:
        return '{}({}, {}, {})'.format(
            self.__class__.__name__,
            repr(self.command),
            self.exit_code,
            repr(self.output),
        )


class TimeoutError(Exception):
    def __init__(self, command: Union[Tuple[str, ...], str], timeout: int) -> None:
        self.command = command
        self.timeout = timeout


class UnexpectedShowBlocksOutputFormatError(Exception):
    def __init__(self, output: str) -> None:
        self.output = output


class UnexpectedProposeOutputFormatError(Exception):
    def __init__(self, output: str) -> None:
        self.output = output


def extract_block_count_from_show_blocks(show_blocks_output: str) -> int:
    lines = show_blocks_output.splitlines()
    prefix = 'count: '
    interesting_lines = [l for l in lines if l.startswith(prefix)]
    if len(interesting_lines) != 1:
        raise UnexpectedShowBlocksOutputFormatError(show_blocks_output)
    line = interesting_lines[0]
    count = line[len(prefix):]
    try:
        result = int(count)
    except ValueError:
        raise UnexpectedShowBlocksOutputFormatError(show_blocks_output)
    return result


def extract_block_hash_from_propose_output(propose_output: str):
    """We're getting back something along the lines of:

    Response: Success! Block a91208047c... created and added.\n
    """
    match = re.match(r'Response: Success! Block ([0-9a-f]+)\.\.\. created and added.', propose_output.strip())
    if match is None:
        raise UnexpectedProposeOutputFormatError(propose_output)
    return match.group(1)


class Node:
    def __init__(self, container: "Container", deploy_dir: str, docker_client: "DockerClient", timeout: int,
                 network: str) -> None:
        self.container = container
        self.local_deploy_dir = deploy_dir
        self.remote_deploy_dir = rnode_deploy_dir
        self.name = container.name
        self.docker_client = docker_client
        self.timeout = timeout
        self.network = network
        self.terminate_background_logging_event = threading.Event()
        self.background_logging = LoggingThread(
            container=container,
            logger=logging.getLogger('peers'),
            terminate_thread_event=self.terminate_background_logging_event,
        )
        self.background_logging.start()

    def __repr__(self) -> str:
        return '<Node(name={})>'.format(repr(self.name))

    def logs(self) -> str:
        return self.container.logs().decode('utf-8')

    def get_rnode_address(self) -> str:
        log_content = self.logs()
        m = re.search("Listening for traffic on (rnode://.+@{name}\\?protocol=\\d+&discovery=\\d+)\\.$".format(name=self.container.name),
                      log_content,
                      re.MULTILINE | re.DOTALL)
        if m is None:
            raise RNodeAddressNotFoundError()
        address = m.group(1)

        logging.info("Bootstrap address: {}".format(repr(address)))
        return address

    def get_metrics(self) -> Tuple[int, str]:
        cmd = 'curl -s http://localhost:40403/metrics'
        return self.exec_run(cmd=cmd)

    def get_metrics_strict(self):
        return self.shell_out('curl', '-s', 'http://localhost:40403/metrics')

    def cleanup(self) -> None:
        self.container.remove(force=True, v=True)
        self.terminate_background_logging_event.set()
        self.background_logging.join()

    def deploy_contract(self, contract: str) -> Tuple[int, str]:
        cmd = '{rnode_binary} deploy --from "0x1" --phlo-limit 1000000 --phlo-price 1 --nonce 0 {rnode_deploy_dir}/{contract}'.format(
            rnode_binary=rnode_binary,
            rnode_deploy_dir=rnode_deploy_dir,
            contract=contract
        )
        return self.exec_run(cmd)

    def propose_contract(self) -> Tuple[int, str]:
        return self.exec_run('{} propose'.format(rnode_binary))

    def show_blocks(self) -> Tuple[int, str]:
        return self.exec_run('{} show-blocks'.format(rnode_binary))

    def show_blocks_with_depth(self, depth: int) -> Tuple[int, str]:
        return self.exec_run(f'{rnode_binary} show-blocks --depth {depth}')

    def get_blocks_count(self, depth: int) -> int:
        _, show_blocks_output = self.show_blocks_with_depth(depth)
        return extract_block_count_from_show_blocks(show_blocks_output)

    def get_block(self, block_hash: str) -> str:
        return self.call_rnode('show-block', block_hash, stderr=False)

    # deprecated, don't use, why? ask @adaszko
    def exec_run(self, cmd: Union[Tuple[str, ...], str], stderr=True) -> Tuple[int, str]:
        queue: Queue = Queue(1)

        def execution():
            r = self.container.exec_run(cmd, stderr=stderr)
            queue.put((r.exit_code, r.output.decode('utf-8')))

        process = Process(target=execution)

        logging.info("{}: {}".format(self.name, cmd))

        process.start()

        try:
            exit_code, output = queue.get(timeout=self.timeout)
            if exit_code != 0:
                logging.warning("{}: {} exited with {}".format(self.name, cmd, exit_code))
            logging.debug('output={}'.format(repr(output)))
            return exit_code, output
        except Empty:
            process.terminate()
            process.join()
            raise TimeoutError(cmd, self.timeout)

    def shell_out(self, *cmd: str, stderr=True) -> str:
        exit_code, output = self.exec_run(cmd, stderr=stderr)
        if exit_code != 0:
            raise NonZeroExitCodeError(command=cmd, exit_code=exit_code, output=output)
        return output

    def call_rnode(self, *node_args: str, stderr: bool = True) -> str:
        return self.shell_out(rnode_binary, *node_args, stderr=stderr)

    def eval(self, rho_file_path: str) -> str:
        return self.call_rnode('eval', rho_file_path)

    def deploy(self, rho_file_path: str) -> str:
        return self.call_rnode('deploy', '--from=0x1', '--phlo-limit=1000000', '--phlo-price=1', '--nonce=0',
                               rho_file_path)

    def deploy_string(self, rholang_code: str) -> str:
        quoted_rholang = shlex.quote(rholang_code)
        return self.shell_out('sh', '-c', 'echo {quoted_rholang} >/tmp/deploy_string.rho && {rnode_binary} deploy --phlo-limit=10000000000 --phlo-price=1 /tmp/deploy_string.rho'.format(
            rnode_binary=rnode_binary,
            quoted_rholang=quoted_rholang,
        ))

    def propose(self) -> str:
        output = self.call_rnode('propose', stderr=False)
        block_hash = extract_block_hash_from_propose_output(output)
        return block_hash

    def repl(self, rholang_code: str, stderr: bool = False) -> str:
        quoted_rholang_code = shlex.quote(rholang_code)
        return self.shell_out('sh',
                              '-c',
                              'echo {quoted_rholang_code} | {rnode_binary} repl'.format(quoted_rholang_code=quoted_rholang_code,rnode_binary=rnode_binary),
                              stderr=stderr)

    def generate_faucet_bonding_deploys(self, bond_amount: int, private_key: str, public_key: str) -> str:
        return self.call_rnode('generateFaucetBondingDeploys',
            '--amount={}'.format(bond_amount),
            '--private-key={}'.format(private_key),
            '--public-key={}'.format(public_key),
            '--sig-algorithm=ed25519',
        )

    def cat_forward_file(self, public_key: str) -> str:
        return self.shell_out('cat', '/opt/docker/forward_{}.rho'.format(public_key))

    def cat_bond_file(self, public_key: str) -> str:
        return self.shell_out('cat', '/opt/docker/bond_{}.rho'.format(public_key))

    __timestamp_rx = "\\d\\d:\\d\\d:\\d\\d\\.\\d\\d\\d"
    __log_message_rx = re.compile("^{timestamp_rx} (.*?)(?={timestamp_rx})".format(timestamp_rx=__timestamp_rx), re.MULTILINE | re.DOTALL)

    def log_lines(self) -> List[str]:
        log_content = self.logs()
        return Node.__log_message_rx.split(log_content)


class LoggingThread(threading.Thread):
    def __init__(self, terminate_thread_event: "Event", container: "Container", logger: "Logger") -> None:
        super().__init__()
        self.terminate_thread_event = terminate_thread_event
        self.container = container
        self.logger = logger

    def run(self) -> None:
        containers_log_lines_generator = self.container.logs(stream=True, follow=True)
        try:
            while True:
                if self.terminate_thread_event.is_set():
                    break
                line = next(containers_log_lines_generator)
                self.logger.info('\t{}: {}'.format(self.container.name, line.decode('utf-8').rstrip()))
        except StopIteration:
            pass


def make_container_command(container_command: str, container_command_options: Dict):
    opts = ['{} {}'.format(option, argument) for option, argument in container_command_options.items()]
    result = '{} {}'.format(container_command, ' '.join(opts))
    return result


def create_node_container(
    *,
    docker_client: "DockerClient",
    name: str,
    network: str,
    bonds_file: str,
    container_command: str,
    container_command_options: Dict,
    rnode_timeout: int,
    extra_volumes: List[str],
    allowed_peers: Optional[List[str]],
    cpuset_cpus: str,
    image: str = DEFAULT_IMAGE,
    mem_limit: Optional[str] = None,
) -> Node:
    assert isinstance(name, str)
    assert '_' not in name, 'Underscore is not allowed in host name'
    deploy_dir = make_tempdir("rchain-integration-test")

    hosts_allow_file_content = \
        "ALL:ALL" if allowed_peers is None else "\n".join("ALL: {}".format(peer) for peer in allowed_peers)

    hosts_allow_file = make_tempfile("hosts-allow-{}".format(name), hosts_allow_file_content)
    hosts_deny_file = make_tempfile("hosts-deny-{}".format(name), "ALL: ALL")

    command = make_container_command(container_command, container_command_options)

    env = {}
    java_options = os.environ.get('_JAVA_OPTIONS')
    if java_options is not None:
        env['_JAVA_OPTIONS'] = java_options
    logging.debug('Using _JAVA_OPTIONS: {}'.format(java_options))

    volumes = [
        "{}:/etc/hosts.allow".format(hosts_allow_file),
        "{}:/etc/hosts.deny".format(hosts_deny_file),
        "{}:{}".format(bonds_file, rnode_bonds_file),
        "{}:{}".format(deploy_dir, rnode_deploy_dir),
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

    node = Node(
        container,
        deploy_dir,
        docker_client,
        rnode_timeout,
        network,
    )

    return node


def get_absolute_path_for_mounting(relative_path: str, mount_dir: Optional[str]=None)-> str:
    """Drone runs each job in a new Docker container FOO. That Docker
    container has a new filesystem. Anything in that container can read
    anything in that filesystem. To read files from HOST, it has to be shared
    though, so let's share /tmp:/tmp. You also want to start new Docker
    containers, so you share /var/run/docker.sock:/var/run/docker.sock. When
    you start a new Docker container from FOO, it's not in any way nested. You
    just contact the Docker daemon running on HOST via the shared docker.sock.
    So when you start a new image from FOO, the HOST creates a new Docker
    container BAR with brand new filesystem. So if you tell Docker from FOO to
    mount /MOUNT_DIR:/MOUNT_DIR from FOO to BAR, the Docker daemon will actually mount
    /MOUNT_DIR from HOST to BAR, and not from FOO to BAR.
    """

    if mount_dir is not None:
        return os.path.join(mount_dir, relative_path)
    return os.path.abspath(os.path.join('resources', relative_path))


def make_bootstrap_node(
    *,
    docker_client: "DockerClient",
    network: str,
    bonds_file: str,
    key_pair: "KeyPair",
    rnode_timeout: int,
    allowed_peers: Optional[List[str]] = None,
    image: str = DEFAULT_IMAGE,
    cpuset_cpus: str = "0",
    mem_limit: Optional[str] = None,
    cli_options: Optional[Dict] = None,
    container_name: Optional[str] = None,
    mount_dir: Optional[str] = None,
) -> Node:
    key_file = get_absolute_path_for_mounting("bootstrap_certificate/node.key.pem", mount_dir=mount_dir)
    cert_file = get_absolute_path_for_mounting("bootstrap_certificate/node.certificate.pem", mount_dir=mount_dir)

    name = "{node_name}.{network_name}".format(
        node_name='bootstrap' if container_name is None else container_name,
        network_name=network,
    )
    container_command_options = {
        "--port":                   40400,
        "--standalone":             "",
        "--validator-private-key":  key_pair.private_key,
        "--validator-public-key":   key_pair.public_key,
        "--has-faucet":             "",
        "--host":                   name,
    }

    if cli_options is not None:
        container_command_options.update(cli_options)

    volumes = [
        "{}:{}".format(cert_file, rnode_certificate),
        "{}:{}".format(key_file, rnode_key)
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


def make_peer_name(network: str, i: Union[int, str]) -> str:
    return "peer{i}.{network}".format(i=i, network=network)


def create_peer(
    *,
    docker_client: "DockerClient",
    network: str,
    name: str,
    bonds_file: str,
    rnode_timeout: int,
    bootstrap: Node,
    key_pair: "KeyPair",
    allowed_peers: Optional[List[str]] = None,
    image: str = DEFAULT_IMAGE,
    cpuset_cpus: str = "0",
    mem_limit: Optional[str] = None,
) -> Node:
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


@contextlib.contextmanager
def started_peer(startup_timeout: int, **kwargs) -> Generator['Node', None, None]:
    peer = create_peer(**kwargs)
    try:
        wait_for_node_started(peer, startup_timeout)
        yield peer
    finally:
        peer.cleanup()


def create_peer_nodes(
    *,
    docker_client: "DockerClient",
    bootstrap: Node,
    network: str,
    bonds_file: str,
    key_pairs: List["KeyPair"],
    rnode_timeout: int,
    allowed_peers: Optional[List[str]] = None,
    image: str = DEFAULT_IMAGE,
    mem_limit: Optional[str] = None,
    cpuset_cpus: str = "0",
) -> List[Node]:
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


@contextlib.contextmanager
def docker_network(docker_client: "DockerClient") -> Generator[str, None, None]:
    network_name = "rchain-{}".format(random_string(5).lower())

    docker_client.networks.create(network_name, driver="bridge")

    try:
        yield network_name
    finally:
        for network in docker_client.networks.list():
            if network_name == network.name:
                logging.info("Removing docker network {}".format(network.name))
                network.remove()


@contextlib.contextmanager
def bootstrap_node(docker: "DockerClient", docker_network: str, timeout: int, validators_data: "ValidatorsData", *, container_name: Optional[str] = None, cli_options: Optional[Dict] = None, mount_dir: Optional[str] = None) -> Generator[Node, None, None]:
    node = make_bootstrap_node(
        docker_client=docker,
        network=docker_network,
        bonds_file=validators_data.bonds_file,
        key_pair=validators_data.bootstrap_keys,
        rnode_timeout=timeout,
        container_name=container_name,
        mount_dir=mount_dir,
    )
    try:
        yield node
    finally:
        node.cleanup()


@contextlib.contextmanager
def start_bootstrap(docker_client: "DockerClient", node_start_timeout: int, node_cmd_timeout: int, validators_data: "ValidatorsData", *, container_name: Optional[str] = None, cli_options:Optional[Dict] = None, mount_dir: Optional[str] = None) -> Generator[Node, None, None]:
    with docker_network(docker_client) as network:
        with bootstrap_node(docker_client, network, node_cmd_timeout, validators_data, container_name=container_name, cli_options=cli_options, mount_dir=mount_dir) as node:
            wait_for_node_started(node, node_start_timeout)
            yield node
