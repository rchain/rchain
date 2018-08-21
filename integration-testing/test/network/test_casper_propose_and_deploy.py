from tools.random import random_string
from tools.docker import run_cmd, list_containers
import re
import logging
from delayed_assert import expect, assert_expectations

class rnode:
    binary='/opt/docker/bin/rnode'

    @staticmethod
    def deploy_cmd(f):
        return rnode.binary + f' deploy --from "0x1" --phlo-limit 0 --phlo-price 0 --nonce 0 {f}'

    propose_cmd = binary + " propose"

    show_blocks_cmd = binary + " show-blocks"

class node:
    log_message_rx = re.compile("^\d*:\d*:\d*\.\d* (.*?)$", re.MULTILINE | re.DOTALL)

    @staticmethod
    def received_block_rx(expected_content):
        return re.compile(f"^.* CASPER: Received Block #\d+ \((.*?)\.\.\.\).*?{expected_content}.*$")

    @staticmethod
    def added_block_rx(block_id):
        return re.compile(f"^.* CASPER: Added {block_id}\.\.\.\s*$")

def test_casper_propose_and_deploy(docker, converged_network):
    """
    This test represents an integration test that deploys a contract and then checks
    if all the nodes have received the block containing the contract.
    """

    token_size = 20

    hello_rho = '/opt/docker/examples/tut-hello.rho'

    sed_cmd = f"sed -i -e 's/Joe/{expected_string}/g' {hello_rho}"

    for container in converged_network.containers:

        expected_string = f"[{test_container.name}:{random_string(token_size)}]"
        logging.info(f"Run test on container {test_container.name}. Expected string: {expected_string}")

        try:
            run_cmd(sed_cmd)

            run_cmd(rnode.deploy_cmd(hello_rho))

            print("Propose to blockchain previously deployed smart contracts.")

            run_cmd(rnode.propose_cmd)

            print("Allow for logs to fill out from last propose if needed")
        except Exception as e:
            print(e)

        time.sleep(5)

        retval=0

        print(f"Check all peer logs for blocks containing {expected_string}")

        other_containers = [d
                            for d in list_containers(docker, converged_network.network)
                            if container.name != container.name]

        for container in other_containers:

            log_content = container.logs().decode('utf-8')
            logs = node.log_message_rx.split(log_content)
            blocks_received_ids = [match.group(1) for match in [node.received_block_rx(expected_string).match(log) for log in logs] if match]

            if blocks_received_ids:
                print(f"Container: {container.name}: Received blocks found for {expected_string}: {blocks_received_ids}")


                assert len(blocks_received_ids) == 1, f"Too many blocks received: {blocks_received_ids}"

                block_id = blocks_received_ids[0]

                blocks_added = [match.group(0) for match in [node.added_block_rx(block_id).match(log) for log in logs] if match]

                expect(not "RuntimeException" in line, f"Container {container.name} error in log line: {line}")
                if blocks_added:
                    print(f"Container: {container.name}: Added block found for {blocks_received_ids}: {blocks_added}. Success!")
                else:
                    print(f"Container: {container.name}: Added blocks not found for {blocks_received_ids}. FAILURE!")
                    retval = retval + 1

            else:
                print(f"Container: {container.name}: String {expected_string} NOT found in output. FAILURE!")
