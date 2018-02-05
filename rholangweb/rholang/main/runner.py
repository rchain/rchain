"""runner -- compile and run rholang programs

Integration Testing
-------------------

  $ python runner.py hello.rbl

or

  $ python runner.py --text "print(4)"


Make sure the `settings` module is in your PYTHONPATH.


Static Type Checking
--------------------

Static types can be checked using mypy; for example:

  $ MYPYPATH=..:. mypy --strict runner.py

"""

# To maintain object capability discipline,
# don't import any powerful objects at module scope.
# See also `__main__` and `_script` below.
#
# Powerful objects are those that do I/O or otherwise
# depend on ambient authority (such as username or platform)
# or introduce non-determinism (such as threading or a clock).
#
# When a powerful class is used for static type checking,
# use a `T` suffix to hide its use as a constructor.
from pathlib import Path as PathT  # not for use as constructor
from resource import RLIMIT_STACK
from subprocess import PIPE, Popen as PopenT
from subprocess import CalledProcessError, SubprocessError, TimeoutExpired
from tempfile import TemporaryDirectory as TemporaryDirectoryT
from typing import Any, Callable, List, Optional, Text, Tuple, Union, cast

import logging  # exception to ocap discipline

log = logging.getLogger(__name__)


class ConfigurationError(Exception):
    """Server admin fault; e.g. problem with settings.py.
    """


class JavaRequired(ConfigurationError):
    """Ensure `java` is in your $PATH.
    """


class UserError(Exception):
    """Client problem.
    """


class CompileError(UserError):
    """Problem compiling rholang program.
    """


class RunError(UserError):
    """Problem running rbl code on rosette VM.
    """


ArgT = Union[bytes, Text, PathT]
OutErr = Tuple[bytes, bytes]


RunnerT = Callable[[List[ArgT], Optional[bytes]], OutErr]


class Compiler(object):
    """Access to rholang compiler.
    """

    out_suffix = '.rbl'

    def __init__(self, run_jar: RunnerT) -> None:
        self.__run_jar = run_jar

    @classmethod
    def jar_runner(self,
                   jarRd: PathT,
                   timeout: float,
                   Popen: Callable[..., PopenT]) -> RunnerT:
        """Attenuate Popen to only run a jar for `timeout` sec.

        Ensure that `java -version` works and that the compiler jar is
        readable.

        In Keeping with the best practice that constructors don't fail,
        this is separate from the constructor.
        """
        try:
            proc = Popen(['java', '-version'], stderr=PIPE)
            _, err = proc.communicate(timeout=timeout)
            if proc.returncode != 0:
                raise JavaRequired(err)
            log.info('java version:\n%s', err.decode('us-ascii'))
        except OSError as oops:
            raise JavaRequired() from oops

        try:
            jarRd.open()
        except OSError as oops:
            raise ConfigurationError() from oops

        def run_jar(argv: List[ArgT], input: Optional[bytes]) -> OutErr:
            argv = [cast(ArgT, 'java'), '-jar', jarRd] + list(argv)
            log.info('running: %s', argv)
            proc = Popen([str(a) for a in argv], stdout=PIPE, stderr=PIPE)
            out, err = proc.communicate(input, timeout=timeout)
            if proc.returncode != 0:
                raise CompileError(err.decode('utf-8'))
            return out, err
        return run_jar

    def compile_file(self, infile: PathT) -> PathT:
        try:
            self.__run_jar([infile], None)
        except SubprocessError as oops:
            raise CompileError() from oops
        return infile.with_suffix(self.out_suffix)

    def compile_text(self, text: Text, work: PathT,
                     filename: str='playground.rho') -> Text:
        # ISSUE: we assume the compiler produces foo.rbl from foo.rho
        infile = work / filename

        with infile.open('w') as fp:
            fp.write(text)
        outfile = self.compile_file(infile)
        outcode = outfile.open().read()  # type: Text
        log.debug('compiled code: %s ...', outcode[:40])
        return outcode


class VM(object):
    """Access to rhoVM.
    """
    def __init__(self, library: PathT, runVM: RunnerT) -> None:
        self.__runVM = runVM
        self.__library = library

    @classmethod
    def program_runner(cls, program: PathT, timeout: float, stack: int,
                       setrlimit: Callable[[int, Tuple[int, int]], None],
                       Popen: Callable[..., PopenT]) -> RunnerT:
        """Attenuate Popen to run `program` for `timeout` sec. with
        stack limited to `stack` bytes.
        """

        def limit_child_stack() -> None:
            setrlimit(RLIMIT_STACK, (stack, stack))

        def run_program(argv: List[ArgT], input: Optional[bytes]) -> OutErr:
            cmd = [cast(ArgT, program)] + argv
            log.info('cmd: %s', cmd)
            proc = Popen([str(a) for a in cmd],
                         preexec_fn=limit_child_stack,
                         stdin=PIPE, stdout=PIPE, stderr=PIPE)
            out, err = proc.communicate(input, timeout=timeout)
            if proc.returncode != 0:
                log.error('subprocess returned %d: %s',
                          proc.returncode, err.decode('utf-8'))
                raise RunError('%d: %s'
                               % (proc.returncode, err.decode('utf-8')))
            return out, err

        return run_program

    def run_repl(self, rbl: Text,
                 verbose: bool=False) -> Tuple[Text, Text, Text]:
        try:
            flags = [cast(ArgT, "--boot-dir"), self.__library] + (
                ["--verbose"] if verbose else [])
            out, err = self.__runVM(flags, rbl.encode('utf-8'))
        except CalledProcessError as oops:
            raise RunError(oops) from oops
        except TimeoutExpired as oops:
            raise RunError(
                'Timeout Expired: %s\n'
                'Try checking for infinite loops.' % (oops))
        return self._cleanup(out.decode('utf-8'))

    @classmethod
    def _cleanup(cls, text: Text) -> Tuple[Text, Text, Text]:
        warnings = ''
        preamble = ''
        if text.startswith('**warning'):
            line, text = text.split('\n', 1)
            warnings += line + '\n'
            if text.startswith('setting ESS_SYSDIR'):
                line, text = text.split('\n', 1)
                warnings += line + '\n'
        while text and not text.startswith('rosette>'):
            line, text = text.split('\n', 1)
            preamble += line + '\n'
        return warnings, preamble, text


def _integration_test(argv: List[str],
                      cfg: Any,
                      Path: Callable[..., PathT],
                      TemporaryDirectory: Callable[..., TemporaryDirectoryT],
                      setrlimit: Callable[[int, Tuple[int, int]], None],
                      Popen: Callable[..., PopenT]) -> None:
    """Compile and run a rholang program.

    See module docstring for usage.
    """
    runj = Compiler.jar_runner(Path(cfg.COMPILER_JAR), cfg.TIMEOUT, Popen)
    c = Compiler(runj)

    log.debug('integration test argv: %s text? %s', argv, '--text' in argv)
    if '--text' in argv:
        with TemporaryDirectory(prefix='rholang') as tmp:
            rbl = c.compile_text(argv[-1], Path(tmp))
        log.info("compiled text:\n%s", rbl)
    else:
        out = c.compile_file(Path(argv[-1]))
        log.info("compiled file: %s", out)
        rbl = out.open().read()

    vm = VM(Path(cfg.VM_LIBRARY),
            VM.program_runner(Path(cfg.VM_PROGRAM),
                              cfg.TIMEOUT, cfg.STACKLIMIT,
                              setrlimit, Popen))

    warnings, preamble, session = vm.run_repl(rbl)
    log.info('vm result:\n%s', session)


if __name__ == '__main__':
    def _script() -> None:
        """Exercise authority granted by use as a top-level script.

        See module docstring for usage.
        """
        # Note that these names don't become available at module scope.
        from resource import setrlimit
        from subprocess import Popen
        from sys import argv
        from tempfile import TemporaryDirectory
        from pathlib import Path

        import settings as cfg

        logging.basicConfig(level=logging.DEBUG)
        _integration_test(argv, cfg,
                          Path, TemporaryDirectory,
                          setrlimit, Popen)

    _script()
