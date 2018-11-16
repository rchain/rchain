#!/usr/bin/env python3.6
"""
This is a simple script to boot a rchain p2p network using bootstrap/peer/bond named containers
It deletes bootstrap/peer named containers before it creates them leaving other named containers

Usage:
 boot-p2p.py [options] [--help]

Options:
 -c --cpuset-cpus=C   set docker cpuset-cpus for all nodes. Allows limiting execution in specific CPUs
                      [default: 0]
 -i --image=I         source repo for docker image
                      [default: coop.rchain/rnode:latest]
 -m --memory=M        set docker memory limit for all nodes
                      [default: 2048m]
 -n --network=N       set docker network name
                      [default: rchain.coop]
 -p --peers-amount=N  set total amount of peers for network
                      [default: 2]
 -r --remove          forcibly remove containers that start with bootstrap and peer associated to network name
 --bonds=FILE         set the bond file of the genesis process
 --wallet=FILE        set the wallet file of the genesis process
 --genesis            set if start with the genesis process
                      ISSUE: not used???
 --sigs=N             set the required signatures , this equals the number of nodes bonded at genesis
                      [default: 0]
 --has-faucet         set if the chain support has-faucet
 --debug              verbose logging
 -h --help            Show this help.

Return code of 0 is success on test and 1 is fail.

Example below shows how to boot network with 3 nodes, including bootstrap, and run specific test

sudo ./boot-p2p.py -m 34360m -c 1 -p 3  --genesis --sigs 2 --bonds <bond_file_path> --wallet <wallet_file_path> --has-faucet  -i rchain-integration-testing:latest --remove

"""
# This requires Python 3.6 to be installed for f-string. Install dependencies via pip
# python3.6 -m pip install docker ed25519 docopt

from collections import namedtuple
import logging

from docopt import docopt

log = logging.getLogger(__name__)


def main(argv, cwd,
         here, temp,
         randint, create_keypair,
         sleep, time,
         docker_from_env):
    """Main program"""
    args = Flags(docopt(__doc__, argv=argv[1:]),
                 {'--peers-amount': int, '--sigs': int})

    if args.sigs > args.peers_amount:
        raise Exception('Sigs should be lower than peer amount')

    client = docker_from_env()

    # Check is the docker network exist and create one if not.
    networks = [network.name for network in client.networks.list()]
    if args.network not in networks:
        client.networks.create(args.network)
    if args.remove:
        # only removes boostrap/peer.rchain.coop or .network nodes
        remove_resources_by_network(client.containers, args.network)

    image = DockerImage(client.containers,
                        args.image, args.cpuset_cpus, args.memory, args.network)

    bond_keys = [KeyPair.encode(create_keypair())
                 for _ in range(args.sigs)]
    peer_keys = [KeyPair.encode(create_keypair())
                 for _ in range(args.peers_amount - args.sigs)]
    network = P2PNetwork(args.network, time, sleep)
    network.boot(image, temp, args.has_faucet, bond_keys, peer_keys,
                 wallets=cwd / args.wallet if args.wallet else None,
                 other_bonds=cwd / args.bonds if args.bonds else None)


class Flags(object):
    def __init__(self, opts,
                 typed={}):
        self.opts = opts
        self.typed = typed

    def __getattr__(self, name):
        opts = self.opts
        for k in [name, '--' + name.replace('_', '-')]:
            if k in opts:
                parser = self.typed.get(k, lambda x: x)
                return parser(self.opts[k])
        else:
            raise AttributeError((name, opts))


class KeyPair(namedtuple("KeyPair", ["private_key", "public_key"])):
    @classmethod
    def encode(cls, key_pair):
        signing_key, verifying_key = key_pair
        encoded_private_key = signing_key.to_ascii(encoding="base16").decode('utf-8')
        encoded_public_key = verifying_key.to_ascii(encoding="base16").decode('utf-8')
        return cls(private_key=encoded_private_key, public_key=encoded_public_key)


def remove_resources_by_network(containers, args_network):
    """Remove bootstrap/peer resources in network name."""
    log.info(f"Removing bootstrap/peer named containers for docker network {args_network}")
    for container in containers.list(all=True, filters={"name": f"peer\\d.{args_network}"}):
        log.info('removing %s', container)
        container.remove(force=True, v=True)
    for container in containers.list(all=True, filters={"name": f"bootstrap.{args_network}"}):
        log.info('removing %s', container)
        container.remove(force=True, v=True)
    for container in containers.list(all=True, filters={"name": f"bond\\d.{args_network}"}):
        log.info('removing %s', container)
        container.remove(force=True, v=True)
    # client.volumes.prune() # Removes unused volumes


class P2PNetwork(object):
    def __init__(self, network, time, sleep):
        self.network = network
        self.__time = time
        self.__sleep = sleep

    def boot(self, image, temp, has_faucet, bond_keys, peer_keys,
             wallets, other_bonds=None):
        bootstrap = BootstrapNode(self.network, other_bonds, wallets, has_faucet,
                                  timestamp=int(self.__time() * 1000))
        bootstrap.run(image, temp)

        log.info('sleep 30: Give bootstrap node an early start')
        self.__sleep(30)

        log.info("Starting bonded nodes")
        log.info("Create and run bonded nodes to connect via bootstrap.")
        for i, key_pair in enumerate(bond_keys):
            peer = PeerNode("bond", self.network, i, key_pair)
            peer.run(image, bootstrap)

        log.info("Starting peer nodes.")
        for i, key_pair in enumerate(peer_keys):
            peer = PeerNode("peer", self.network, i, key_pair)
            peer.run(image, bootstrap)

    def bonds_file(self, temp, bond_key_pairs, randint,
                   other_bonds=None):
        log.info('Make bonds file including {other_bonds} and {len(bond_key_pairs)} generated keys.')
        if other_bonds:
            with other_bonds.open() as bond_f:
                init = [line.split() for line in bond_f.readlines()]
        else:
            init = []

        bonds = init + [(key_pair.public_key, randint(50, 1000))
                        for key_pair in bond_key_pairs]

        temp_bonds_file = temp / f'bonds_{randint(10000, 99999)}'

        with temp_bonds_file.open('w') as f:
            for public_key, bond in bonds:
                f.write(f'{public_key} {bond}\n')

        return temp_bonds_file


class DockerImage(object):
    """Access to create containers based on an image.
    """
    def __init__(self, containers,
                 image, cpuset, memory, network):
        self.__containers = containers
        self.image = image
        self.cpuset = cpuset
        self.memory = memory
        self.network = network

    def run(self, name, volumes={}, command_args=[]):
        detail = dict(
            image=self.image,
            name=name,
            user='root',
            detach=True,
            cpuset_cpus=self.cpuset,
            mem_limit=self.memory,
            network=self.network,
            volumes=volumes,
            command=command_args,
            hostname=name)
        log.debug('starting: %s', detail)
        return self.__containers.run(**detail)


class RNode(object):
    directory = '/var/lib/rnode'


class BootstrapNode(RNode):
    # Create key/cert pem files to be loaded into rnode volume
    bootstrap_node_demo_key = (
        "-----BEGIN PRIVATE KEY-----\n"
        "MIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgYcybGU15SCs2x+5I\n"
        "JHrzzBHZ0c7k2WwokG6yU754XKKgCgYIKoZIzj0DAQehRANCAAR/MkqpcKUE+NtM\n"
        "d8q7/IPih2vO6oMjm2ltSA2nSrueNd+jpLvxDQpRYScJBDyeylfB1VkPdpw9oqFQ\n"
        "Y5huc38x\n"
        "-----END PRIVATE KEY-----\n"
    )
    bootstrap_node_demo_cert = (
        "-----BEGIN CERTIFICATE-----\n"
        "MIIBXzCCAQKgAwIBAgIIU0qinJbBW5MwDAYIKoZIzj0EAwIFADAzMTEwLwYDVQQD\n"
        "EyhjYjc0YmEwNDA4NTU3NGU5ZjAxMDJjYzEzZDM5ZjBjNzIyMTljNWJiMB4XDTE4\n"
        "MDYxMjEzMzEyM1oXDTE5MDYxMjEzMzEyM1owMzExMC8GA1UEAxMoY2I3NGJhMDQw\n"
        "ODU1NzRlOWYwMTAyY2MxM2QzOWYwYzcyMjE5YzViYjBZMBMGByqGSM49AgEGCCqG\n"
        "SM49AwEHA0IABH8ySqlwpQT420x3yrv8g+KHa87qgyObaW1IDadKu54136Oku/EN\n"
        "ClFhJwkEPJ7KV8HVWQ92nD2ioVBjmG5zfzEwDAYIKoZIzj0EAwIFAANJADBGAiEA\n"
        "62Po1SVQyJB/2UeG5B9O1oTTlhYrLvLTWH24YiH4U4kCIQDrPa3Qop3yq83Egdq0\n"
        "VkEqI2rycmgp03DXsStJ7IGdBQ==\n"
        "-----END CERTIFICATE-----\n"
    )

    def __init__(self, network, bonds, wallets, has_faucet, timestamp):
        self.network = network
        self.name = name = f"bootstrap.{self.network}"

        command, volumes = self.genesis(bonds, wallets)

        if has_faucet:
            command = command + ["--has-faucet"]

        self.command = [
            'run', "--standalone",
            "--host", name,
            "--deploy-timestamp", f"{timestamp}",
            "--map-size", "17179869184",
        ] + command

        self.genesis_volumes = volumes

    @property
    def address(self):
        return f'rnode://cb74ba04085574e9f0102cc13d39f0c72219c5bb@bootstrap.{self.network}?protocol=40400&discovery=40404'

    def key_volumes(self, temp):
        tmp_file_key = temp / 'node.key.pem'
        tmp_file_cert = temp / 'node.certificate.pem'
        with tmp_file_key.open('w') as f:
            f.write(self.bootstrap_node_demo_key)
        with tmp_file_cert.open('w') as f:
            f.write(self.bootstrap_node_demo_cert)

        volumes = {
            str(tmp_file_cert): {
                "bind": f'{RNode.directory}/node.certificate.pem',
                "mode": 'rw'},
            str(tmp_file_key): {"bind": f'{RNode.directory}/node.key.pem',
                                "mode": 'rw'}
        }

        return volumes

    def run(self, image, temp):
        """Create bootstrap node."""

        log.info("Starting bootstrap node.")
        log.info(f"creating {self.name}")

        volumes = dict(self.key_volumes(temp),
                       **self.genesis_volumes)
        return image.run(self.name, volumes, self.command)

    @classmethod
    def genesis(cls, sigs, bonds=None, wallets=None):
        container_bonds_file = f'{RNode.directory}/genesis/bonds.txt'
        container_wallets_file = f'{RNode.directory}/genesis/wallets.txt'

        command = []
        volumes = {}

        if bonds or sigs:
            command = command + ["--bonds-file", container_bonds_file]
            volumes.update({str(bonds): {"bind": container_bonds_file, "mode": 'rw'}})
        if wallets:
            command = command + ["--wallets-file", container_wallets_file]
            volumes.update({str(wallets): {"bind": container_wallets_file, "mode": 'rw'}})

        return command, volumes


class PeerNode(RNode):
    def __init__(self, name_prefix, network, i, key_pair):
        name = f"{name_prefix}{i}.{network}"
        log.info(
            f"creating {name_prefix} node {name} with private key:{key_pair.private_key} and public key:{key_pair.public_key}")
        self.name = name
        self._key_pair = key_pair

    def run(self, image, bootstrap):
        peer_prefix_command = ['run', '--bootstrap', bootstrap.address]

        keys_command = ["--validator-private-key", self._key_pair.private_key,
                        "--validator-public-key", self._key_pair.public_key,
                        "--host", self.name]
        return image.run(name=self.name,
                         command_args=peer_prefix_command + keys_command)


if __name__ == "__main__":
    def _script():
        # Access ambient authority only when invoked as a script.
        from pathlib import Path
        from random import randint
        from sys import argv
        from time import sleep, time

        from docker import from_env
        from ed25519 import create_keypair

        logging.basicConfig(
            format='%(asctime)s: %(message)s',
            datefmt='%H:%M:%S',
            level=logging.DEBUG if '--debug' in argv else logging.INFO)

        main(argv, randint=randint,
             sleep=sleep, time=time,
             cwd=Path('.'),
             here=Path(__file__).resolve().parent,
             temp=Path('/tmp'),
             create_keypair=create_keypair,
             docker_from_env=from_env)

    _script()
