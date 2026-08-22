import os
from pathlib import Path
from typing import Iterable

import yaml

WORKFLOW_PROJECT_PATH = '.github/workflows/continuous-integration.yml'
REMAINDER = 'REMAINDER'


def get_project_root() -> Path:
    """Returns project root path from PROJECT_ROOT environment variable or
    falls back to current working directory"""
    return Path(os.getenv('PROJECT_ROOT', os.getcwd()))


def read_workflow() -> dict:
    """Reads continuous-integration.yml workflow as dict"""
    workflow_path = get_project_root() / WORKFLOW_PROJECT_PATH
    with open(workflow_path) as f:
        return yaml.safe_load(f)


def get_test_selections(job_name: str) -> Iterable[str]:
    workflow = read_workflow()
    job = workflow['jobs'][job_name]

    try:
        selections = job['strategy']['matrix']['tests']
    except KeyError:
        return []

    remainder_found = False

    for test_sel in selections:
        if test_sel == REMAINDER:
            if not remainder_found:
                remainder_found = True
                continue
            raise Exception(f"Two {REMAINDER} test selections found")
        yield test_sel
