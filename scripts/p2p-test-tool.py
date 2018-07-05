#!/usr/bin/env python3.6
# This is a simple script to help with p2p network boot/testing.
# This requires Python 3.6 to be installed for f-string. Install dependencies via pip
# python3.6 -m pip install docker argparse pexpect requests
# Return code of 0 is success on test and 1 is fail.
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
                    dest='peers_amount',
                    type=int,
                    default="2",
                    help="set total amount of peers for network")
parser.add_argument("--prompt",
                    dest='prompt',
                    type=str,
                    default="rholang $ ",
                    help="set REPL prompt")
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
                    help="set repl load repetition peers_amount for loops")
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
parser.add_argument("-t", "--tests",
                    dest='run_tests',
                    action='store_true',
                    help="only run tests")
parser.add_argument("-T", "--tests-to-run",
                    dest='tests',
                    type=str,
                    nargs='+',
                    default=['network_sockets', 'count', 'eval', 'repl', 'errors', 'RuntimeException'],
                    help="run these tests in this order")
# Print -h/help if no args
if len(sys.argv)==1:
    parser.print_help(sys.stderr)
    sys.exit(1)


# Define globals
args = parser.parse_args()
client = docker.from_env()
RNODE_CMD = '/opt/docker/bin/rnode'


def main():
    """Main program"""
    if args.run_tests == True:
        run_tests()
        return
    if args.logs == True:
        show_logs()
        return
    if args.remove == True:
        remove_resources_by_network(args.network)
        return
    if args.deploy_demo == True: # deploy casper demo
        deploy_demo()
        return
    if args.boot == True:
        remove_resources_by_network(args.network)
        boot_p2p_network()
        if not args.skip_convergence_test == True:
            for container in client.containers.list(all=True, filters={"name":f'bootstrap.{args.network}'}):
                check_network_convergence(container)
        deploy_demo()
        if args.tests:
            run_tests()
            return


def run_tests():
    notices ={} 
    notices['fail'] = []
    notices['pass'] = []
    if not client.containers.list(all=True, filters={"name":f".{args.network}"}): # return if empty
        return 

    for test in args.tests:
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

    print("=======================SHOW LOGS===========================")
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


def show_logs():
    for container in client.containers.list(all=True, filters={"name":f".{args.network}"}):
        print(f"Showing logs for {container.name}.")
        r = container.logs().decode('utf-8')
        print(r)


def boot_p2p_network():
    try:
        client.networks.create(args.network, driver="bridge")
        print("Starting bootstrap node.")
        create_bootstrap_node()
        time.sleep(10) # give bootstrap node an early start
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
    peers_metric_expected = args.peers_amount
    timeout = 200
    count = 0

    while count < 200:
        cmd = f'curl -s {container.name}:9095'
        r = container.exec_run(cmd=cmd).output.decode('utf-8')
        print(r)
        print(f"checking {count} of {timeout} seconds")
        for line in r.splitlines():
            if line == f"peers {args.peers_amount}.0":
                print("Network converged.")
                return 0
        time.sleep(10)
        count += 10
    print("Timeout of {timeout} seconds reached ... exiting network convergence pre tests probe.")
    return 1 


def remove_resources_by_network(args_network):
    """Remove resources by network name."""
    print(f"Removing resources for docker network {args_network}")
    for container in client.containers.list(all=True, filters={"name":f".{args_network}"}):
            container.remove(force=True, v=True)

    for network in client.networks.list():
        if args_network == network.name:
            print(f"removing {network.name}")
            network.remove()
    # client.volumes.prune() # removes unused volumes
    return 0


def create_bootstrap_node():
    """Create bootstrap node."""

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
        volumes={
                tmp_file_cert: {'bind': f'{args.rnode_directory}/node.certificate.pem', 'mode': 'rw'}, \
                tmp_file_key: {'bind': f'{args.rnode_directory}/node.key.pem', 'mode': 'rw'}, \
                bootstrap_node['volume'].name: {'bind': args.rnode_directory, 'mode': 'rw'} \
        }, \
        command=f"{args.bootstrap_command} --host {bootstrap_node['name']}", \
        hostname=bootstrap_node['name'])
    print("Installing additonal packages on container.")
    r = container.exec_run(cmd='apt-get update', user='root').output.decode("utf-8")
    r = container.exec_run(cmd='apt-get -yq install curl', user='root').output.decode("utf-8")
    r = container.exec_run(cmd='apt-get -yq install nmap', user='root').output.decode("utf-8")
    return 0


def create_peer_nodes():
    """Create peer nodes."""
    print("Start peer nodes to connect via bootstrap.")
    for i in range(args.peers_amount):
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
            volumes=[f"{peer_node[i]['volume'].name}:{args.rnode_directory}"], \
            command=f"{args.peer_command} --host {peer_node[i]['name']}", \
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

            cmd = (f"sudo docker run -u root --rm -it -v {repl_node[i]['volume'].name}:{args.rnode_directory} "
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
        if f"Peers: {args.peers_amount}." in line:
            print(line)
            retval = 0 
    return retval 


if __name__ == "__main__":
    main()
