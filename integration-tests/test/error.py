from typing import Tuple

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
