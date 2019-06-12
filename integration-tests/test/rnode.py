import re
import os
import queue
import shlex
import string
import shutil
import logging
from logging import (Logger)
import threading
from threading import Event
import contextlib
from multiprocessing import Queue, Process
from collections import defaultdict
from typing import (
    Dict,
    List,
    Tuple,
    Optional,
    Generator,
    AbstractSet,
    Set)

from docker.client import DockerClient
from docker.models.containers import Container
from docker.models.containers import ExecResult

from .common import (
    KeyPair,
    make_tempdir,
    make_tempfile,
    TestingContext,
    NonZeroExitCodeError,
    GetBlockError,
)
from .wait import (
    wait_for_node_started,
    wait_for_approved_block_received_handler_state,
)


DEFAULT_IMAGE = os.environ.get("DEFAULT_IMAGE", "rchain-integration-tests:latest")
_PB_REPEATED_STR_SEP = "#$"

rnode_binary = '/opt/docker/bin/rnode'
rnode_directory = "/var/lib/rnode"
rnode_deploy_dir = "{}/deploy".format(rnode_directory)
rnode_bonds_file = '{}/genesis/bonds.txt'.format(rnode_directory)
rnode_wallets_file = '{}/genesis/wallets.txt'.format(rnode_directory)
rnode_certificate = '{}/node.certificate.pem'.format(rnode_directory)
rnode_key = '{}/node.key.pem'.format(rnode_directory)


class RNodeAddressNotFoundError(Exception):
    def __init__(self, regex: str) -> None:
        super().__init__()
        self.regex = regex


class CommandTimeoutError(Exception):
    def __init__(self, command: Tuple[str, ...], timeout: int) -> None:
        super().__init__()
        self.command = command
        self.timeout = timeout


class UnexpectedShowBlocksOutputFormatError(Exception):
    def __init__(self, output: str) -> None:
        super().__init__()
        self.output = output


class UnexpectedProposeOutputFormatError(Exception):
    def __init__(self, output: str) -> None:
        super().__init__()
        self.output = output

class UnexpectedDeployOutputFormatError(Exception):
    def __init__(self, output: str) -> None:
        super().__init__()
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


def parse_show_blocks_key_value_line(line: str) -> Tuple[str, str]:
    match = re.match(r'(?P<key>[^:]*): "?(?P<value>.*(?<!"))', line.strip())
    if match is None:
        raise UnexpectedShowBlocksOutputFormatError(line)
    return (match.group('key'), match.group('value'))


def parse_show_blocks_output(show_blocks_output: str) -> List[Dict[str, str]]:
    result = []

    lines = show_blocks_output.splitlines()

    i = 0
    while True:
        if i >= len(lines):
            break
        if lines[i].startswith('------------- block '):
            block = {}
            j = i + 1
            while True:
                if j >= len(lines):
                    break
                if lines[j].strip() == "":
                    break
                key, value = parse_show_blocks_key_value_line(lines[j])
                block[key] = value
                j += 1
            result.append(block)
            i = j
        else:
            i += 1

    return result


def parse_show_block_output(show_block_output: str) -> Dict[str, str]:
    result: Dict[str, str] = {}

    lines = show_block_output.splitlines()
    bonds_validator = []
    deploy_cost = []
    for line in lines:
        if line.startswith('status:') or line.startswith('blockInfo {') or line.startswith('}'):
            continue
        if line.strip() == '':
            continue
        key, value = parse_show_blocks_key_value_line(line)
        if key == "bondsValidatorList":
            validator_hash = value.strip('"')
            bonds_validator.append(validator_hash)
        elif key == "deployCost":
            deploy_cost.append(value.strip('"'))
        else:
            result[key] = value
    result['bondsValidatorList'] = _PB_REPEATED_STR_SEP.join(bonds_validator)
    result['deployCost'] = _PB_REPEATED_STR_SEP.join(deploy_cost)
    return result


def extract_validator_stake_from_bonds_validator_str(out_put: str) -> Dict[str, float]:
    validator_stake_dict = {}
    validator_stake_list = out_put.split(_PB_REPEATED_STR_SEP)
    for validator_stake in validator_stake_list:
        validator, stake = validator_stake.split(': ')
        stake_f = float(stake)
        validator_stake_dict[validator] = stake_f
    return validator_stake_dict


def extract_block_hash_from_propose_output(propose_output: str) -> str:
    """We're getting back something along the lines of:

    Response: Success! Block a91208047c... created and added.\n
    """
    match = re.match(r'Response: Success! Block ([0-9a-f]+)\.\.\. created and added.', propose_output.strip())
    if match is None:
        raise UnexpectedProposeOutputFormatError(propose_output)
    return match.group(1)


def extract_validator_stake_from_deploy_cost_str(output: str) -> Dict[str, float]:
    deploy_cost_dict: Dict[str, float] = defaultdict(lambda: 0)
    deploy_cost_list = output.split(_PB_REPEATED_STR_SEP)
    for deploy_cost_str in deploy_cost_list:
        match = re.match(r'User: (?P<user>[a-zA-Z0-9]*), Cost: (?P<cost>[0-9]*) DeployData \#(?P<timestamp>[0-9]*) -- .', deploy_cost_str)
        if match:
            deploy_cost_dict[match.group('user')] = int(match.group('cost'))
    return deploy_cost_dict


class Node:
    def __init__(self, *, container: Container, deploy_dir: str, command_timeout: int, network: str) -> None:
        self.container = container
        self.local_deploy_dir = deploy_dir
        self.remote_deploy_dir = rnode_deploy_dir
        self.name = container.name
        self.command_timeout = command_timeout
        self.network = network
        self.terminate_background_logging_event = threading.Event()
        self.background_logging = LoggingThread(
            container=container,
            logger=logging.getLogger('peers'),
            terminate_thread_event=self.terminate_background_logging_event,
        )
        self.background_logging.setDaemon(True)
        self.background_logging.start()

    def __repr__(self) -> str:
        return '<Node(name={})>'.format(repr(self.name))

    def logs(self) -> str:
        return self.container.logs().decode('utf-8')

    def get_rnode_address(self) -> str:
        log_content = self.logs()
        regex = "Listening for traffic on (rnode://.+@{name}\\?protocol=\\d+&discovery=\\d+)\\.$".format(name=self.container.name)
        match = re.search(regex, log_content, re.MULTILINE | re.DOTALL)
        if match is None:
            raise RNodeAddressNotFoundError(regex)
        address = match.group(1)
        return address

    def get_metrics(self) -> str:
        return self.shell_out('curl', '-s', 'http://localhost:40403/metrics')

    def get_connected_peers_metric_value(self) -> str:
        try:
            return self.shell_out('sh', '-c', 'curl -s http://localhost:40403/metrics | grep ^rchain_comm_rp_connect_peers\\ ')
        except NonZeroExitCodeError as e:
            if e.exit_code == 1:
                return ''
            raise

    def cleanup(self) -> None:
        self.container.remove(force=True, v=True)
        self.terminate_background_logging_event.set()
        self.background_logging.join()

    def show_blocks_with_depth(self, depth: int) -> str:
        return self.rnode_command('show-blocks', '--depth', str(depth))

    def show_block(self, hash: str) -> str:
        return self.rnode_command('show-block', hash)

    def get_blocks_count(self, depth: int) -> int:
        show_blocks_output = self.show_blocks_with_depth(depth)
        return extract_block_count_from_show_blocks(show_blocks_output)

    def show_blocks_parsed(self, depth: int) -> List[Dict[str, str]]:
        show_blocks_output = self.show_blocks_with_depth(depth)
        return parse_show_blocks_output(show_blocks_output)

    def show_block_parsed(self, hash: str) -> Dict[str, str]:
        show_block_output = self.show_block(hash)
        return parse_show_block_output(show_block_output)

    def get_block(self, block_hash: str) -> str:
        try:
            return self.rnode_command('show-block', block_hash, stderr=False)
        except NonZeroExitCodeError as e:
            raise GetBlockError(command=e.command, exit_code=e.exit_code, output=e.output)

    # Too low level -- do not use directly.  Prefer shell_out() instead.
    def _exec_run_with_timeout(self, cmd: Tuple[str, ...], stderr: bool = True) -> Tuple[int, str]:
        control_queue: queue.Queue = Queue(1)

        def command_process() -> None:
            exec_result: ExecResult = self.container.exec_run(cmd, stderr=stderr)
            control_queue.put((exec_result.exit_code, exec_result.output.decode('utf-8')))

        process = Process(target=command_process)
        logging.info("COMMAND {} {}".format(self.name, cmd))
        process.start()

        try:
            exit_code, output = control_queue.get(True, self.command_timeout)
        except queue.Empty:
            raise CommandTimeoutError(cmd, self.command_timeout)
        finally:
            process.terminate()

        if exit_code != 0:
            for line in output.splitlines():
                logging.info('{}: {}'.format(self.name, line))
            logging.warning("EXITED {} {} {}".format(self.name, cmd, exit_code))
        else:
            for line in output.splitlines():
                logging.debug('{}: {}'.format(self.name, line))
            logging.debug("EXITED {} {} {}".format(self.name, cmd, exit_code))
        return exit_code, output

    def shell_out(self, *cmd: str, stderr: bool = True) -> str:
        exit_code, output = self._exec_run_with_timeout(cmd, stderr=stderr)
        if exit_code != 0:
            raise NonZeroExitCodeError(command=cmd, exit_code=exit_code, output=output)
        return output

    def rnode_command(self, *node_args: str, stderr: bool = True) -> str:
        return self.shell_out(rnode_binary, *node_args, stderr=stderr)

    def eval(self, rho_file_path: str) -> str:
        return self.rnode_command('eval', rho_file_path)

    def deploy(self, rho_file_path: str, private_key: str) -> str:
        return extract_deploy_id_from_deploy_output(self.rnode_command('deploy', '--private-key={}'.format(private_key), '--phlo-limit=1000000', '--phlo-price=1', rho_file_path, stderr=False))

    def get_vdag(self) -> str:
        return self.rnode_command('vdag')

    def get_mvdag(self) -> str:
        return self.rnode_command('mvdag', stderr=False)

    def get_parsed_mvdag(self) -> Dict[str, Set[str]]:
        return parse_mvdag_str(self.get_mvdag())

    def deploy_string(self, rholang_code: str, private_key: str) -> str:
        quoted_rholang = shlex.quote(rholang_code)
        deploy_out = self.shell_out('sh', '-c', 'echo {quoted_rholang} >/tmp/deploy_string.rho && {rnode_binary} deploy --private-key={private_key} --phlo-limit=10000000000 --phlo-price=1 /tmp/deploy_string.rho'.format(
            rnode_binary=rnode_binary,
            quoted_rholang=quoted_rholang,
            private_key=private_key
        ), stderr=False)
        return extract_deploy_id_from_deploy_output(deploy_out)

    def find_deploy(self, deploy_id: str) -> Dict[str, str]:
        return parse_show_block_output(self.rnode_command("find-deploy", "--deploy-id", deploy_id, stderr=False))

    def propose(self) -> str:
        output = self.rnode_command('propose', stderr=False)
        block_hash = extract_block_hash_from_propose_output(output)
        return block_hash

    def repl(self, rholang_code: str, stderr: bool = False) -> str:
        quoted_rholang_code = shlex.quote(rholang_code)
        output = self.shell_out(
            'sh',
            '-c',
            'echo {quoted_rholang_code} | {rnode_binary} repl'.format(quoted_rholang_code=quoted_rholang_code,rnode_binary=rnode_binary),
            stderr=stderr,
        )
        return output

    def cat_forward_file(self, public_key: str) -> str:
        return self.shell_out('cat', '/opt/docker/forward_{}.rho'.format(public_key))

    def cat_bond_file(self, public_key: str) -> str:
        return self.shell_out('cat', '/opt/docker/bond_{}.rho'.format(public_key))

    __timestamp_rx = "\\d\\d:\\d\\d:\\d\\d\\.\\d\\d\\d"
    __log_message_rx = re.compile("^{timestamp_rx} (.*?)(?={timestamp_rx})".format(timestamp_rx=__timestamp_rx), re.MULTILINE | re.DOTALL)

    def log_lines(self) -> List[str]:
        log_content = self.logs()
        return Node.__log_message_rx.split(log_content)

    def deploy_contract_with_substitution(self, substitute_dict: Dict[str, str], rho_file_path: str, private_key: str) -> str:
        """
        Supposed that you have a contract with content like below.

        new x in { x!("#DATA") }

        If you pass a dict {'#DATA': "123456"} as substitute_dict args in this func,
        this method would substitute the string #DATA in the contract with 123456, which turns out to be

        new x in { x!("123456") }

        And then deploy the contract in the node
        """
        shutil.copyfile(rho_file_path, os.path.join(self.local_deploy_dir, os.path.basename(rho_file_path)))
        container_contract_file_path = os.path.join(self.remote_deploy_dir, os.path.basename(rho_file_path))
        substitute_rules = ';'.join([r's/{}/{}/g'.format(key.replace(r'/', r'\/'), value.replace(r'/', r'\/')) for key, value in substitute_dict.items()])
        self.shell_out(
            'sed',
            '-i',
            '-e', substitute_rules,
            container_contract_file_path,
        )
        self.deploy(container_contract_file_path, private_key)
        block_hash = self.propose()
        return block_hash


class LoggingThread(threading.Thread):
    def __init__(self, terminate_thread_event: Event, container: Container, logger: Logger) -> None:
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
                self.logger.info('{}: {}'.format(self.container.name, line.decode('utf-8').rstrip()))
        except StopIteration:
            pass


def make_container_command(container_command: str, container_command_flags: AbstractSet, container_command_options: Dict) -> str:
    opts = ['{} {}'.format(option, argument) for option, argument in container_command_options.items()]
    flags = ' '.join(container_command_flags)
    result = '{} {} {}'.format(container_command, flags, ' '.join(opts))
    return result


def make_node(
    *,
    docker_client: DockerClient,
    name: str,
    network: str,
    bonds_file: str,
    container_command: str,
    container_command_flags: AbstractSet,
    container_command_options: Dict,
    command_timeout: int,
    extra_volumes: List[str],
    allowed_peers: Optional[List[str]],
    image: str = DEFAULT_IMAGE,
    mem_limit: Optional[str] = None,
    wallets_file: Optional[str] = None,
) -> Node:
    assert isinstance(name, str)
    assert '_' not in name, 'Underscore is not allowed in host name'
    deploy_dir = make_tempdir("rchain-integration-test")

    hosts_allow_file_content = \
        "ALL:ALL" if allowed_peers is None else "\n".join("ALL: {}".format(peer) for peer in allowed_peers)

    hosts_allow_file = make_tempfile("hosts-allow-{}".format(name), hosts_allow_file_content)
    hosts_deny_file = make_tempfile("hosts-deny-{}".format(name), "ALL: ALL")

    command = make_container_command(container_command, container_command_flags, container_command_options)

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

    if wallets_file is not None:
        volumes.append('{}:{}'.format(wallets_file, rnode_wallets_file))

    logging.info('STARTING %s %s', name, command)
    container = docker_client.containers.run(
        image,
        name=name,
        user='root',
        detach=True,
        mem_limit=mem_limit,
        network=network,
        volumes=volumes + extra_volumes,
        command=command,
        hostname=name,
        environment=env,
    )

    node = Node(
        container=container,
        deploy_dir=deploy_dir,
        command_timeout=command_timeout,
        network=network,
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
    docker_client: DockerClient,
    network: str,
    bonds_file: str,
    keypair: KeyPair,
    command_timeout: int,
    allowed_peers: Optional[List[str]] = None,
    mem_limit: Optional[str] = None,
    cli_flags: Optional[AbstractSet] = None,
    cli_options: Optional[Dict] = None,
    mount_dir: Optional[str] = None,
    wallets_file: Optional[str] = None,
) -> Node:
    key_file = get_absolute_path_for_mounting("bootstrap_certificate/node.key.pem", mount_dir=mount_dir)
    cert_file = get_absolute_path_for_mounting("bootstrap_certificate/node.certificate.pem", mount_dir=mount_dir)

    container_name = make_bootstrap_name(network)

    container_command_flags = set([
        "--standalone",
        "--prometheus",
        "--no-upnp",
        "--allow-private-addresses"
    ])

    container_command_options = {
        "--port":                   40400,
        "--validator-private-key":  keypair.private_key,
        "--validator-public-key":   keypair.public_key,
        "--host":                   container_name,
    }

    if cli_flags is not None:
        container_command_flags.update(cli_flags)

    if cli_options is not None:
        container_command_options.update(cli_options)

    volumes = [
        "{}:{}".format(cert_file, rnode_certificate),
        "{}:{}".format(key_file, rnode_key)
    ]

    container = make_node(
        docker_client=docker_client,
        name=container_name,
        network=network,
        bonds_file=bonds_file,
        container_command='run',
        container_command_flags=container_command_flags,
        container_command_options=container_command_options,
        command_timeout=command_timeout,
        extra_volumes=volumes,
        allowed_peers=allowed_peers,
        mem_limit=mem_limit if mem_limit is not None else '4G',
        wallets_file=wallets_file,
    )
    return container


def make_container_name(network_name: str, name: str) -> str:
    return "{network_name}.{name}".format(network_name=network_name, name=name)


def make_bootstrap_name(network_name: str) -> str:
    return make_container_name(network_name=network_name, name='bootstrap')


def make_peer_name(network_name: str, name: str) -> str:
    if name.isdigit():
        actual_name = 'peer{}'.format(name)
    else:
        actual_name = name
    return make_container_name(network_name=network_name, name=actual_name)


def make_peer(
    *,
    docker_client: DockerClient,
    network: str,
    name: str,
    bonds_file: str,
    command_timeout: int,
    bootstrap: Node,
    keypair: KeyPair,
    allowed_peers: Optional[List[str]] = None,
    mem_limit: Optional[str] = None,
    wallets_file: Optional[str] = None,
    cli_flags: Optional[AbstractSet] = None,
    cli_options: Optional[Dict] = None,
) -> Node:
    assert isinstance(name, str)
    assert '_' not in name, 'Underscore is not allowed in host name'
    name = make_peer_name(network, name)

    bootstrap_address = bootstrap.get_rnode_address()

    container_command_flags = set([
        "--prometheus",
        "--no-upnp",
        "--allow-private-addresses"
    ])

    if cli_flags is not None:
        container_command_flags.update(cli_flags)

    container_command_options = {
        "--bootstrap":              bootstrap_address,
        "--validator-private-key":  keypair.private_key,
        "--validator-public-key":   keypair.public_key,
        "--host":                   name,
    }

    if cli_options is not None:
        container_command_options.update(cli_options)

    container = make_node(
        docker_client=docker_client,
        name=name,
        network=network,
        bonds_file=bonds_file,
        container_command='run',
        container_command_flags=container_command_flags,
        container_command_options=container_command_options,
        command_timeout=command_timeout,
        extra_volumes=[],
        allowed_peers=allowed_peers,
        mem_limit=mem_limit if not None else '4G',
        wallets_file=wallets_file,
    )
    return container


@contextlib.contextmanager
def started_peer(
    *,
    context: TestingContext,
    network: str,
    name: str,
    bootstrap: Node,
    keypair: KeyPair,
    wallets_file: Optional[str] = None,
    cli_flags: Optional[AbstractSet] = None,
    cli_options: Optional[Dict] = None,
) -> Generator[Node, None, None]:
    peer = make_peer(
        docker_client=context.docker,
        network=network,
        name=name,
        bonds_file=context.bonds_file,
        bootstrap=bootstrap,
        keypair=keypair,
        command_timeout=context.command_timeout,
        wallets_file=wallets_file,
        cli_flags=cli_flags,
        cli_options=cli_options,
    )
    try:
        wait_for_node_started(context, peer)
        yield peer
    finally:
        peer.cleanup()


@contextlib.contextmanager
def bootstrap_connected_peer(
    *,
    context: TestingContext,
    bootstrap: Node,
    name: str,
    keypair: KeyPair,
) -> Generator[Node, None, None]:
    with started_peer(
        context=context,
        network=bootstrap.network,
        name=name,
        bootstrap=bootstrap,
        keypair=keypair,
    ) as peer:
        wait_for_approved_block_received_handler_state(context, peer)
        yield peer


def create_peer_nodes(
    *,
    docker_client: DockerClient,
    bootstrap: Node,
    network: str,
    bonds_file: str,
    key_pairs: List[KeyPair],
    command_timeout: int,
    allowed_peers: Optional[List[str]] = None,
    mem_limit: Optional[str] = None,
) -> List[Node]:
    assert len(set(key_pairs)) == len(key_pairs), "There shouldn't be any duplicates in the key pairs"

    if allowed_peers is None:
        allowed_peers = [bootstrap.name] + [make_peer_name(network, str(i)) for i in range(0, len(key_pairs))]

    result = []
    try:
        for i, keypair in enumerate(key_pairs):
            peer_node = make_peer(
                docker_client=docker_client,
                network=network,
                name=str(i),
                bonds_file=bonds_file,
                command_timeout=command_timeout,
                bootstrap=bootstrap,
                keypair=keypair,
                allowed_peers=allowed_peers,
                mem_limit=mem_limit if mem_limit is not None else '4G',
            )
            result.append(peer_node)
    except:
        for node in result:
            node.cleanup()
        raise
    return result


def make_random_network_name(context: TestingContext, length: int) -> str:
    return ''.join(context.random_generator.choice(string.ascii_lowercase) for m in range(length))


@contextlib.contextmanager
def docker_network(context: TestingContext, docker_client: DockerClient) -> Generator[str, None, None]:
    network_name = "rchain-{}".format(make_random_network_name(context, 5))
    docker_client.networks.create(network_name, driver="bridge")
    try:
        yield network_name
    finally:
        for network in docker_client.networks.list():
            if network_name == network.name:
                network.remove()


@contextlib.contextmanager
def started_bootstrap(
    *,
    context: TestingContext,
    network: str,
    mount_dir: str = None,
    cli_flags: Optional[AbstractSet] = None,
    cli_options: Optional[Dict] = None,
    wallets_file: Optional[str] = None,
) -> Generator[Node, None, None]:
    bootstrap_node = make_bootstrap_node(
        docker_client=context.docker,
        network=network,
        bonds_file=context.bonds_file,
        keypair=context.bootstrap_keypair,
        command_timeout=context.command_timeout,
        mount_dir=mount_dir,
        cli_flags=cli_flags,
        cli_options=cli_options,
        wallets_file=wallets_file,
    )
    try:
        wait_for_node_started(context, bootstrap_node)
        yield bootstrap_node
    finally:
        bootstrap_node.cleanup()


@contextlib.contextmanager
def docker_network_with_started_bootstrap(context: TestingContext, cli_flags: Optional[AbstractSet] = None) -> Generator[Node, None, None]:
    with docker_network(context, context.docker) as network:
        with started_bootstrap(context=context, network=network, mount_dir=context.mount_dir, cli_flags=cli_flags) as bootstrap:
            wait_for_approved_block_received_handler_state(context, bootstrap)
            yield bootstrap


@contextlib.contextmanager
def ready_bootstrap(
    context: TestingContext,
    cli_flags: Optional[AbstractSet] = None,
    cli_options: Optional[Dict] = None,
    wallets_file: Optional[str] = None,
) -> Generator[Node, None, None]:
    with docker_network(context, context.docker) as network:
        with started_bootstrap(context=context, network=network, mount_dir=context.mount_dir, cli_flags=cli_flags, cli_options=cli_options, wallets_file=wallets_file) as node:
            yield node


def parse_mvdag_str(mvdag_output: str) -> Dict[str, Set[str]]:
    dag_dict: Dict[str, Set[str]] = defaultdict(set)

    lines = mvdag_output.splitlines()
    for line in lines:
        parent_hash, child_hash = line.split(' ')
        dag_dict[parent_hash].add(child_hash)
    return dag_dict

def extract_deploy_id_from_deploy_output(deploy_output: str) -> str:
    match = re.match(r'Response: Success!\nDeployId is: ([0-9a-f]+)', deploy_output.strip())
    if match is None:
        raise UnexpectedDeployOutputFormatError(deploy_output)
    return match.group(1)
