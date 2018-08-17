import logging

def run_cmd(docker_container, cmd):
    logging.info(f"{docker_container.name}: Execute <{cmd}>")
    r = docker_container.exec_run(['sh', '-c', cmd])
    output = r.output.decode('utf-8')

    logging.info(f"{docker_container.name}: Finish <{cmd}>. Exit Code: {r.exit_code}")
    return (r.exit_code, output)