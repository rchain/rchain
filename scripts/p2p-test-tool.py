#!/usr/bin/env python3.6
# This is a simple script to help with p2p network boot/testing.
# This requires Python 3.6 to be installed for f-string. Install dependencies via pip
# python3.6 -m pip install docker argparse pexpect requests
# Return code of 0 is success on test and 1 is fail.
# Example below shows how to boot network with 3 nodes, including bootstrap, and run specific test
# ./p2p-test-tool.py -b -m 2048m -i rchain/rnode:dev -T propose -t
from pexpect import replwrap
import subprocess
import argparse
import docker
import os
import tempfile
import requests
import re
import time
import sys
import random


parser = argparse.ArgumentParser(
             formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("-b", "--boot",
                    action='store_true',
                    help="boot network by creating resources and starting services by network name")
parser.add_argument("--bootstrap-command",
                    dest='bootstrap_command',
                    type=str,
                    default="run --port 30304 --standalone",
                    help="bootstrap container run command")
parser.add_argument("-c", "--cpuset-cpus",
                    dest='cpuset_cpus',
                    type=str,
                    default="0",
                    help="set docker cpuset-cpus for all nodes. Allows limiting execution in specific CPUs")
parser.add_argument("-d", "--deploy-demo",
                    dest='deploy_demo',
                    action='store_true',
                    help="deploy casper demo")
parser.add_argument("-i", "--image",
                    dest='image',
                    type=str,
                    default="coop.rchain/rnode:latest",
                    help="source repo for docker image")
parser.add_argument("-l", "--logs",
                    action='store_true',
                    help="show all node logs")
parser.add_argument("-m", "--memory",
                    dest='memory',
                    type=str,
                    default="1024m",
                    help="set docker memory limit for all nodes")
parser.add_argument("-n", "--network",
                    dest='network',
                    type=str,
                    default="rchain.coop",
                    help="set docker network name")
parser.add_argument("--peer-command",
                    dest='peer_command',
                    type=str,
                    default="run --bootstrap rnode://cb74ba04085574e9f0102cc13d39f0c72219c5bb@bootstrap.rchain.coop:30304",
                    help="peer container run command")
parser.add_argument("-p", "--peers-amount",
                    dest='peer_amount',
                    type=int,
                    default="2",
                    help="set total amount of peers for network")
parser.add_argument("--prompt",
                    dest='prompt',
                    type=str,
                    default="rholang $ ",
                    help="set REPL prompt")
parser.add_argument("--propose-loop-amount",
                    dest='propose_loop_amount',
                    type=int,
                    default="3",
                    help="set amount of times propose test will loop before checking logs for issues")
parser.add_argument("--repl-commands",
                    dest='repl_cmds',
                    type=str,
                    nargs='+',
                    default=['5',
                        '@"stdout"!("foo")',
                        '@"listCh"!([1, 2, 3]) | for(@list <- @"listCh"){ match list { [a, b, c] => { @"stdout"!(a) } } }'],
                    help="set repl commands to run as a list")
parser.add_argument("--repl-load-repetitions",
                    dest='repl_load_repetitions',
                    type=int,
                    default=50,
                    help="set repl load repetition peer_amount for loops")
parser.add_argument("--rnode-directory",
                    dest='rnode_directory',
                    type=str,
                    default="/var/lib/rnode",
                    help="rnode container root directory on each container")
parser.add_argument("-r", "--remove",
                    action='store_true',
                    help="forcibly remove all container resources associated to network name")
parser.add_argument("-s", "--skip-convergence-test",
                    dest="skip_convergence_test",
                    action='store_true',
                    help="skip network convergence test")
parser.add_argument("--test-performance",
                    dest='test_performance',
                    action='store_true',
                    help="Runs infite loops of load generating node functions so you can test performance by looking at metrics.")
parser.add_argument("-t", "--tests",
                    dest='run_tests',
                    action='store_true',
                    help="only run tests")
parser.add_argument("-T", "--tests-to-run",
                    dest='tests_to_run',
                    type=str,
                    nargs='+',
                    default=['network_sockets', 'count', 'eval', 'repl', 'propose', 'errors', 'RuntimeException'],
                    help="run these tests in this order")
# Print -h/help if no args
if len(sys.argv)==1:
    parser.print_help(sys.stderr)
    sys.exit(1)


# Define globals
args = parser.parse_args()
client = docker.from_env()
RNODE_CMD = '/opt/docker/bin/rnode'
# bonds_file = f'/tmp/bonds.{args.network}' alternate when dynamic bonds.txt creation/manpiulation file works
bonds_file = dir_path = os.path.dirname(os.path.realpath(__file__)) + '/demo-bonds.txt' 
container_bonds_file = f'{args.rnode_directory}/validators/bonds.txt'


def main():
    """Main program"""
    if args.logs == True:
        show_logs()
        return
    if args.test_performance == True:
        test_performance()
        return
    if args.remove == True:
        remove_resources_by_network(args.network)
        return
    if args.deploy_demo == True: # Deploy casper demo
        deploy_demo()
        return
    if args.boot == True:
        remove_resources_by_network(args.network)
        boot_p2p_network()
        if not args.skip_convergence_test == True:
            for container in client.containers.list(all=True, filters={"name":f'bootstrap.{args.network}'}):
                check_network_convergence(container)
    if args.run_tests == True:
        run_tests()
        return


def run_tests():
    notices ={} 
    notices['fail'] = []
    notices['pass'] = []
    if not client.containers.list(all=True, filters={"name":f".{args.network}"}): # Return if empty
        return 

    for test in args.tests_to_run:
        if test == "network_sockets":
            for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
                if test_network_sockets(container) == 0:
                    notices['pass'].append(f"{container.name}: Metrics API http/tcp/9095 is available.")
                else:
                    notices['fail'].append(f"{container.name}: Metrics API http/tcp/9095 is not available.")
        if test == "errors":
            for container in client.containers.list(all=True, filters={"name":f'peer\d.{args.network}'}):
                print(container.name)
                if test_node_logs_for_errors(container) == 0:
                    notices['pass'].append(f"{container.name}: No errors defined by \"ERROR\" in logs.")
                else:
                    notices['fail'].append(f"{container.name}: Errors defined by \"ERROR\" found in logs.")
        if test == "RuntimeException":
            for container in client.containers.list(all=True, filters={"name":f'peer\d.{args.network}'}):
                print(container.name)
                if test_node_logs_for_RuntimeException(container) == 0:
                    notices['pass'].append(f"{container.name}: No text of \"RuntimeException\" in logs.")
                else:
                    notices['fail'].append(f"{container.name}: Text of \"RuntimeException\" in logs.")
        if test == "count":
            for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
                if test_node_logs_for_correct_peers_count(container) == 0:
                    notices['pass'].append(f"{container.name}: Peers count correct in node logs.")
                else:
                    notices['fail'].append(f"{container.name}: Peers count incorrect in node logs.")
        if test == "propose":
            for container in client.containers.list(all=True, filters={"name":f".{args.network}"}):
                if test_propose(container) == 0:
                    notices['pass'].append(f"{container.name}: Proposal of blocks for deployed contracts worked.")
                else:
                    notices['fail'].append(f"{container.name}: Proposal of blocks for deployed contracts failed.")
        if test == "eval":
            for container in client.containers.list(filters={"name":f"peer\d.{args.network}"}):
                if test_node_eval_of_rholang_files(container) == 0:
                    notices['pass'].append(f"{container.name}: Rholang evaluation of files performed correctly.")
                else:
                    notices['fail'].append(f"{container.name}: Rholang evaluation of files failed.")
        if test == "repl":
            for container in client.containers.list(all=True, filters={"name":f"peer0.{args.network}"}):
                if test_repl_load(container) == 0:
                    notices['pass'].append(f"{container.name}: REPL loader success!")
                else:
                    notices['fail'].append(f"{container.name}: REPL loader failure!")
            time.sleep(10) # allow repl container to stop so it doesn't interfere with other tests

    print("=======================SHOW LOGS===========================")
    print("Dumping logs from nodes in 3 seconds.")
    time.sleep(3)
    show_logs()
    print("====================END OF SHOW LOGS=======================")
    print("===========================================================")
    print("=================TEST SUMMARY RESULTS======================")
    if notices['pass']:
        for notice in notices['pass']:
            print(f"PASS: {notice}")
    if notices['fail']:
        for notice in notices['fail']:
            print(f"FAIL: {notice}")
        print('FAIL: Part or all of tests failed in one or more peer nodes.')
        raise Exception('FAIL: Part or all of tests failed in one or more peer nodes.')
    else:
        print("PASS ALL: All tests successfully passed")
    print("===========================================================")
    print("===========================================================")


def deploy_demo():
    cmd = f"{RNODE_CMD} deploy-demo"
    for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
        try:
            r = container.exec_run(cmd=cmd, user='root', detach=True)
            if r.exit_code:
                raise Exception(f"ERROR: There was an issue executing --deploy-demo command on {container.name}")
        except Exception as e:
            print(e)


def test_node_eval_of_rholang_files(container):
    print(f"Running eval rho file tests of /usr/share/rnode/examples/ on container {container.name}.")
    cmd = f"ls /opt/docker/examples/*.rho"
    r = container.exec_run(['sh', '-c', cmd])
    for file_path in r.output.decode('utf-8').splitlines():
        print(file_path)
        eval_r = container.exec_run(['sh', '-c', f'{RNODE_CMD} eval {file_path}'])
        for line in eval_r.output.decode('utf-8').splitlines():
            if 'ERROR' in line.upper():
                print(line)
                return 1 
    return 0 


def test_propose(container):
    retval = 0
    print(f"Running propose tests after deploy using on container {container.name}.")
    for i in range(1, args.propose_loop_amount+1):
        print(f"Loop number {i} of {args.propose_loop_amount} on {container.name}")

        # Deploy example contracts using 3 random example files
        cmd = "for i in `ls /opt/docker/examples/*.rho | sort -R | tail -n 3`; do /opt/docker/bin/rnode deploy ${i}; done"
        r = container.exec_run(['sh', '-c', cmd])
        for line in r.output.decode('utf-8').splitlines():
            print(line)

        # Propose blocks from example contracts
        cmd = "/opt/docker/bin/rnode propose"
        print("Propose to blockchain previously deployed smart contracts.")

        r = container.exec_run(['sh', '-c', cmd])
        for line in r.output.decode('utf-8').splitlines():
            print(line)

    print("Check all peer logs for casper WARN or ERROR messages")
    time.sleep(5) # Allow for logs to fill out from last propose if needed
    for container in client.containers.list(all=True, filters={"name":f".{args.network}"}):
            #Check logs for warnings(WARN) or errors(ERROR) on CASPER    
            for line in container.logs().decode('utf-8').splitlines():
                if "WARN" in line and "CASPER" in line:
                    print(f"{container.name}: {line}")
                    retval = 1
                if "ERROR" in line and "CASPER" in line:
                    print(f"{container.name}: {line}")
                    retval = 1

    return retval


def show_logs():
    for container in client.containers.list(all=True, filters={"name":f".{args.network}"}):
        print(f"Showing logs for {container.name}.")
        r = container.logs().decode('utf-8')
        print(r)


def create_empty_bonds_file():

    # Create or zero out bonds file so it is empty and can be mounted as volumes by containers 
    try:
        with open(bonds_file, 'w+') as f:
            f.write("")
    except IOError as e:
        print(f"Failed to open or write to file {e}.")


def boot_p2p_network():
    try:
        client.networks.create(args.network, driver="bridge")
        print("Starting bootstrap node.")
        # create_empty_bonds_file() # disabled until python generated keys work
        create_bootstrap_node()
        time.sleep(20) # Give bootstrap node an early start
        print("Starting peer nodes.")
        create_peer_nodes()
        return 0
    except Exception as e:
        print(e)
        return 1


def var_to_docker_file(var, container_name, file_path):
    """Convert variabled to container file."""
    fd, path = tempfile.mkstemp()
    try:
        with os.fdopen(fd, 'w') as tmp:
            tmp.write(var)
    finally:
        subprocess.call(["docker", "cp", path, f"{container_name}:{file_path}"])
        os.remove(path)
    return 0


def check_network_convergence(container):
    print("Check for network convergence via prometheus metrics api before running tests.")
    peers_metric = ''
    peers_metric_expected = args.peer_amount
    timeout = 200
    count = 0

    while count < 200:
        cmd = f'curl -s {container.name}:9095'
        r = container.exec_run(cmd=cmd).output.decode('utf-8')
        print(r)
        print(f"checking {count} of {timeout} seconds")
        for line in r.splitlines():
            if line == f"peers {args.peer_amount}.0":
                print("Network converged.")
                return 0
        time.sleep(10)
        count += 10
    print("Timeout of {timeout} seconds reached ... exiting network convergence pre tests probe.")
    return 1 


def test_performance():
    # Infinite loop for performance testing via metrics.
    # Only run parent script "-p 1" so it runs only on one peer, peer0.rchain.coop.
    print("=====================================================================================")
    print("Preparing to run infinite deploy/propose loop for stress/performance testing and metric collection.")
    print("You will have to cancel or Ctrl-C to exit script.")
    print("=====================================================================================")
    print('Run parent script with "-p 1" if you only want deploy/propose to run from a single node to network nodes')
    print("=====================================================================================")
    print("Grab metrics on peer0 container via:")
    print("sudo docker exec -it peer0.rchain.coop bash -c 'curl 127.0.0.1:9095'")
    print("sudo docker exec -it peer0.rchain.coop bash -c './bin/rnode diagnostics'")
    print("=====================================================================================")
    print("Quick and dirty comparative script. Shows metric lines changed and the values.")
    print("Run last command to see changes from start /tmp/1")
    print("sudo docker exec -it peer0.rchain.coop bash -c 'curl 127.0.0.1:9095' > /tmp/1")
    print("""sudo docker exec -it peer0.rchain.coop bash -c 'curl 127.0.0.1:9095' > /tmp/2 && diff -y --suppress-common-lines /tmp/1 /tmp/2 | tr -d '\\t\\r\\f'  | awk '{print $1" | " $2" | "$4}'""")
    print("=====================================================================================")
    time.sleep(10)
    while True:
        for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
            for i in range(1, args.propose_loop_amount+1):
                print(f"Loop number {i} of {args.propose_loop_amount} on {container.name}")

                # Deploy example contracts using 3 random example files
                cmd = "for i in `ls /opt/docker/examples/*.rho | sort -R | tail -n 3`; do /opt/docker/bin/rnode deploy ${i}; done"
                r = container.exec_run(['sh', '-c', cmd])
                for line in r.output.decode('utf-8').splitlines():
                    print(line)

                # Propose blocks from example contracts
                cmd = "/opt/docker/bin/rnode propose"
                print("Propose to blockchain previously deployed smart contracts.")

                r = container.exec_run(['sh', '-c', cmd])
                for line in r.output.decode('utf-8').splitlines():
                    print(line)


def remove_resources_by_network(args_network):
    """Remove resources by network name."""
    print(f"Removing resources for docker network {args_network}")
    for container in client.containers.list(all=True, filters={"name":f".{args_network}"}):
            container.remove(force=True, v=True)

    for network in client.networks.list():
        if args_network == network.name:
            print(f"removing {network.name}")
            network.remove()
    # client.volumes.prune() # Removes unused volumes
    return 0


def generate_validator_private_key():
    ### Create --validator-private-key and --validator--public-key and add to bonds.txt # ed25519 eventually secp256k1 ###

    # # pynacl for libsodium
    # # import nacl # libsodium/ed25519 support
    # from nacl.public import PrivateKey, PublicKey
    # import nacl.encoding
    # import nacl.signing
    # private_key = PrivateKey.generate()
    # encoded_private_key = private_key.encode(encoder=nacl.encoding.Base16Encoder).decode('utf-8').lower()
    # encoded_public_key = private_key.public_key.encode(encoder=nacl.encoding.Base16Encoder).decode('utf-8').lower()
    # signing_key = nacl.signing.SigningKey.generate() 
    # verify_key = signing_key.verify_key
    # encoded_private_key = signing_key.encode(encoder=nacl.encoding.Base16Encoder).lower()
    # encoded_public_key = verify_key.encode(encoder=nacl.encoding.Base16Encoder).lower() 

    # import ed25519
    # signing_key, verifying_key = ed25519.create_keypair()
    # encoded_private_key = signing_key.to_ascii(encoding="base16").decode('utf-8')
    # encoded_public_key = verifying_key.to_ascii(encoding="base16").decode('utf-8')

    # import ecdsa # secp256k1 suppport

    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    f=open('scripts/demo-validator-private-public-key-pairs.txt')
    lines=f.readlines()
    line_number = random.randint(1,295)
    encoded_private_key = lines[line_number].split()[0]
    encoded_public_key = lines[line_number].split()[1]

    # print(f"Populating bonds file {bonds_file}")
    # bond_weight = random.randint(1,100)
    # line = f"{encoded_public_key} {bond_weight}"
    # print(line)
    # try:
    #     with open(bonds_file, 'a+') as f:
    #         f.write(f"{line}\n")
    # except IOError as e:
    #     print(f"Failed to open or write to file {e}.") 

    return encoded_private_key, encoded_public_key


def populate_bonds_file():
    print(f"Populating bonds file {bonds_file}")
    try:
        with open(bonds_file, 'a+') as f:
            for line in bonds:
                print(line)
                f.write(f"{line}\n")
    except IOError as e:
        print(f"Failed to open or write to file {e}.") 


def create_bootstrap_node():
    """Create bootstrap node."""

    validator_private_key, validator_public_key = generate_validator_private_key()

    # Create key/cert pem files to be loaded into rnode volume
    bootstrap_node_demo_key=(
        "-----BEGIN PRIVATE KEY-----\n"
        "MIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgYcybGU15SCs2x+5I\n"
        "JHrzzBHZ0c7k2WwokG6yU754XKKgCgYIKoZIzj0DAQehRANCAAR/MkqpcKUE+NtM\n"
        "d8q7/IPih2vO6oMjm2ltSA2nSrueNd+jpLvxDQpRYScJBDyeylfB1VkPdpw9oqFQ\n"
        "Y5huc38x\n"
        "-----END PRIVATE KEY-----\n"
    )
    bootstrap_node_demo_cert=(
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
    tmp_file_key = '/tmp/node.key.pem'
    tmp_file_cert = '/tmp/node.certificate.pem'
    with open(tmp_file_key, 'w') as f:
        f.write(bootstrap_node_demo_key)
    with open(tmp_file_cert, 'w') as f:
        f.write(bootstrap_node_demo_cert)
 
    print("Starting bootstrap node.")
    bootstrap_node = {}
    bootstrap_node['name'] = f"bootstrap.{args.network}"
    bootstrap_node['volume'] = client.volumes.create()
    print(f"creating {bootstrap_node['name']}")
    container = client.containers.run(args.image, \
        name=bootstrap_node['name'], \
        user='root', \
        detach=True, \
        cpuset_cpus=args.cpuset_cpus, \
        mem_limit=args.memory, \
        network=args.network, \
        volumes=[
                f"{bootstrap_node['volume'].name}:{args.rnode_directory}", \
                f"{bonds_file}:{container_bonds_file}", \
                f"{tmp_file_cert}:{args.rnode_directory}/node.certificate.pem", \
                f"{tmp_file_key}:{args.rnode_directory}/node.key.pem"
        ],
        # # Alternate volume mount
        # volumes={
        #         tmp_file_cert: {'bind': f'{args.rnode_directory}/node.certificate.pem', 'mode': 'rw'}, \
        #         tmp_file_key: {'bind': f'{args.rnode_directory}/node.key.pem', 'mode': 'rw'}, \
        #         bonds_file: {'bind': container_bonds_file, 'mode': 'rw'}, \
        #         bootstrap_node['volume'].name: {'bind': args.rnode_directory, 'mode': 'rw'} \
        # }, \
        command=f"{args.bootstrap_command} --validator-private-key {validator_private_key} --validator-public-key {validator_public_key} --host {bootstrap_node['name']}", \
        hostname=bootstrap_node['name'])
    print("Installing additonal packages on container.")
    r = container.exec_run(cmd='apt-get update', user='root').output.decode("utf-8")
    r = container.exec_run(cmd='apt-get -yq install curl', user='root').output.decode("utf-8")
    r = container.exec_run(cmd='apt-get -yq install nmap', user='root').output.decode("utf-8")
    return 0


def create_peer_nodes():
    """Create peer nodes."""
    print("Create and run peer nodes to connect via bootstrap.")

    for i in range(args.peer_amount):
        validator_private_key, validator_public_key = generate_validator_private_key()
        peer_node = {}
        peer_node[i] = {}
        peer_node[i]['name'] = f"peer{i}.{args.network}"
        peer_node[i]['volume'] = client.volumes.create()
        print(f"creating {peer_node[i]['name']}")
        container = client.containers.run(args.image, \
            name=peer_node[i]['name'], \
            user='root', \
            detach=True, \
            cpuset_cpus=args.cpuset_cpus, \
            mem_limit=args.memory, \
            network=args.network, \
            volumes=[
                f"{bonds_file}:{container_bonds_file}", \
                f"{peer_node[i]['volume'].name}:{args.rnode_directory}"
            ], \
            command=f"{args.peer_command} --validator-private-key {validator_private_key} --validator-public-key {validator_public_key} --host {peer_node[i]['name']}", \
            hostname=peer_node[i]['name'])

        print("Installing additonal packages on container.")
        r = container.exec_run(cmd='apt-get update', user='root').output.decode("utf-8")
        r = container.exec_run(cmd='apt-get -yq install curl', user='root').output.decode("utf-8")
        r = container.exec_run(cmd='apt-get -yq install nmap', user='root').output.decode("utf-8")
    return 0
      

def test_network_sockets(container):
    print(f"Test metrics api socket for {container.name}")
    try:
        cmd = f"nmap -sS -n -p T:9095 -oG - {container.name}"
        r = container.exec_run(cmd=cmd, user='root').output.decode("utf-8")
        if "9095/open/tcp" in r:
            return 0 
        else:
            return 1 
    except Exception as e:
        print(e)
        return 1 


def test_repl_load(container):
    """Load REPL with commands."""
    print(f"Testing REPL on {container.name} via repl container and Python pexpect.")
    # Remove any existing repl containers if they exist
    for repl_container in client.containers.list(all=True, filters={"name":f"repl\d.{args.network}"}):
        print(f"removing {repl_container.name}")
        repl_container.remove(force=True, v=True)

    # Run repl loader with repetitions and repl commands defined in args
    for i in [0]: # Add more later by var in args if wanted to increase load diversity
        try:
            repl_node = {}
            repl_node[i] = {}
            repl_node[i]['name'] = f"repl{i}.{args.network}"
            repl_node[i]['volume'] = client.volumes.create()

            cmd = (f"sudo docker run -u root -it -v {repl_node[i]['volume'].name}:{args.rnode_directory} "
                   f"--cpuset-cpus={args.cpuset_cpus} --memory={args.memory} --name {repl_node[i]['name']} "
                   f"--network {args.network} {args.image} "
                   f"--grpc-host {container.name} repl")
            print(f"docker repl cmd: {cmd}")
            repl_cmds = args.repl_cmds
            conn = replwrap.REPLWrapper(cmd, args.prompt, None)
            for i in range(args.repl_load_repetitions):
                for repl_cmd in repl_cmds:
                    result = conn.run_command(repl_cmd)
                    print(f"repetition: {i} output: {result}")
                i += 1 

            # Remove repl container after use as we don't want it running 
            for repl_container in client.containers.list(all=True, filters={"name":f"repl\d.{args.network}"}):
                print(f"removing {repl_container.name}")
                repl_container.remove(force=True, v=True)

            return 0 
        except Exception as e:
            print(e)
            return 1 


def test_node_logs_for_errors(container):
    retval = 1 
    print(f"Testing {container.name} node logs for errors.")
    r = container.logs().decode('utf-8')
    if not "ERROR" in r:
        print("PASS: No errors found in logs.")
        retval = 0 
    else:
        print("FAIL: Errors matching ERROR found in logs.")
        for line in r.splitlines():
            if "ERROR" in line:
                print(line)
                retval = 1 
    return retval 


def test_node_logs_for_RuntimeException(container):
    retval = 1 
    print(f"Testing {container.name} node logs for \"java RuntimeException\".")
    r = container.logs().decode('utf-8')
    if not "RuntimeException" in r:
        retval = 0 
    else:
        for line in r.splitlines():
            if "RuntimeException" in line:
                print(line)
                retval = 1 
    return retval 


def test_node_logs_for_correct_peers_count(container):
    retval = 1 
    print(f"Testing {container.name} node logs for correct peers count.")
    r = container.logs()
    for line in r.splitlines():
        line = line.decode('utf-8')
        if f"Peers: {args.peer_amount}." in line:
            print(line)
            retval = 0 
    return retval 


if __name__ == "__main__":
    main()
