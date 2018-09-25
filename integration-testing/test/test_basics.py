import logging
from delayed_assert import expect, assert_expectations
from tools.profiling import profile

@profile
def test_metrics_api_socket(complete_network):
    for node  in complete_network.nodes:
        logging.info(f"Test metrics api socket for {node.name}")
        exit_code, output = node.get_metrics()
        expect(exit_code == 0, "Could not get the metrics for node {node.name}")

    assert_expectations()


@profile
def test_node_logs_for_errors(complete_network):
    for node in complete_network.nodes:
        logging.info(f"Testing {node.name} node logs for errors.")
        logs = node.logs()

        if "ERROR" in logs:
            for line in logs.splitlines():
                if "ERROR" in line:
                    logging.error(f"Error: {line}")
            expect(not "ERROR" in line, f"Node {node.name} error in log line: {line}")

    assert_expectations()

@profile
def test_node_logs_for_RuntimeException(complete_network):
    for node in complete_network.nodes:
        logging.info(f"Testing {node.name} node logs for \"java RuntimeException\".")
        logs = node.logs()


        if "RuntimeException" in logs:
            for line in logs.splitlines():
                if "RuntimeException" in line:
                    logging.error(f"Error: {line}")
            expect(not "RuntimeException" in line, f"Node {node.name} error in log line: {line}")

    assert_expectations()