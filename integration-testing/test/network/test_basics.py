import logging
from delayed_assert import expect, assert_expectations
from tools.profiling import profile

@profile
def test_metrics_api_socket(started_rchain_network):
    for node  in started_rchain_network.nodes:
        logging.info(f"Test metrics api socket for {node.name}")
        cmd = f"nmap -sS -n -p T:40403 -oG - {node.name}"
        r = node.container.exec_run(cmd=cmd, user='root').output.decode("utf-8")
        expect("40403/open/tcp" in r, f"Port 40403/tcp is not open in container {node.name}")

    assert_expectations()


@profile
def test_node_logs_for_errors(converged_network):
    for node in converged_network.nodes:
        logging.info(f"Testing {node.name} node logs for errors.")
        logs = node.logs()

        if "ERROR" in logs:
            for line in logs.splitlines():
                if "ERROR" in line:
                    logging.error(f"Error: {line}")
            expect(not "ERROR" in line, f"Node {node.name} error in log line: {line}")

    assert_expectations()

@profile
def test_node_logs_for_RuntimeException(converged_network):
    for node in converged_network.nodes:
        logging.info(f"Testing {node.name} node logs for \"java RuntimeException\".")
        logs = node.logs()


        if "RuntimeException" in logs:
            for line in logs.splitlines():
                if "RuntimeException" in line:
                    logging.error(f"Error: {line}")
            expect(not "RuntimeException" in line, f"Node {node.name} error in log line: {line}")

    assert_expectations()