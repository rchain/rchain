import os
import time
import logging


def time_func(func, *args, **kwargs):
    """
    measure time taken by the func
    """
    timestamp = time.time()
    func(*args, **kwargs)
    return time.time() - timestamp


def test_eval(bootstrap_node):
    relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    for relative_path in relative_paths:
        full_path = os.path.join('/opt/docker/examples', relative_path)
        bootstrap_node.eval(full_path)

def test_eval_repl_time_consuming(bootstrap_node):
    relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    for relative_path in relative_paths:
        full_path = os.path.join('/opt/docker/examples', relative_path)
        reformat_rho_script = bootstrap_node.shell_out("sed" ,"-e", 's/\/\/.*//', full_path)
        reformat_rho_script = reformat_rho_script.replace("\n", "")

        repl_time = time_func(bootstrap_node.repl, reformat_rho_script)
        logging.info(f"It takes {repl_time} to run {full_path} in repl mode")

        eval_time = time_func(bootstrap_node.eval, full_path)
        logging.info(f"It takes {eval_time} to run {full_path} in eval mode")

        # performance gap between repl and eval should be +- 5% , details see OPS-347
        assert abs(repl_time - eval_time) / eval_time < 0.05
