"""runner -- compile and run rholang programs
"""

from contextlib import contextmanager
from io import StringIO
from os.path import relpath

from pathlib import Path
from subprocess import PIPE, SubprocessError
from sys import stderr
import logging

from rholang.settings import *

log = logging.getLogger(__name__)


class ConfigurationError(Exception):
    pass


class JavaRequired(ConfigurationError):
    pass


class UserError(Exception):
    pass


class CompileError(UserError):
    pass


class RunError(UserError):
    pass


class Compiler(object):

    def __init__(self, run_jar, mkTempDir):
        self.__run_jar = run_jar
        self.__mkTempDir = mkTempDir

    @classmethod
    def make(cls, jarRd, Path, TemporaryDirectory, Popen):
        '''Construct from python stdlib powers.
        '''
        return cls(cls.jar_runner(jarRd, Popen),
                   _mkTempDirMaker(Path, TemporaryDirectory))

    @classmethod
    def jar_runner(self, jarRd, Popen):
        try:
            proc = Popen(['java', '-version'], stderr=PIPE)
            _, err = proc.communicate()
            if proc.returncode != 0:
                raise JavaRequired(diagnostics)
            log.info('java version:\n%s', err.decode('us-ascii'))
        except OSError as oops:
            raise JavaRequired() from oops

        def run_jar(argv):
            argv = ['java', '-jar', str(jarRd)] + list(argv)
            log.info('running: %s', argv)
            proc = Popen(argv, stdout=PIPE, stderr=PIPE)
            out, err = proc.communicate()
            if proc.returncode != 0:
                raise CompileError(err.decode('utf-8'))
            return out, err
        return run_jar

    def compile_file(self, input):
        output = input.with_suffix('.rbl')

        # ISSUE: we assume the compiler produces foo.rbl from foo.rho
        try:
            self.__run_jar([str(input)])
        except SubprocessError as oops:
            raise CompileError(oops.stderr) from oops
        return output

    def compile_text(self, text,
                     filename='playground.rho'):
        with self.__mkTempDir(prefix='rholang') as work:
            input = work / filename
            with input.open('w') as fp:
                fp.write(text)
            output = self.compile_file(input)
            return output.open().read()


class VM(object):
    def __init__(self, runVM):
        self.__runVM = runVM

    @classmethod
    def make(cls, program, library, Popen):
        def runVM(rbl):
            library_rel_path = relpath(os.path.join(library, "boot.rbl"), os.path.dirname(program))
            proc = Popen([program, "-boot", library_rel_path], stdin=PIPE,
                         stdout=PIPE, stderr=PIPE)
            return proc.communicate(rbl.encode('utf-8'))
        return cls(runVM)

    def run_repl(self, rbl):
        try:
            out, err = self.__runVM(rbl)
        except CalledProcessError as oops:
            raise RunError(oops) from oops
        return self._cleanup(out.decode('utf-8'))

    @classmethod
    def _cleanup(cls, text):
        warnings = ''
        preamble = ''
        if text.startswith('**warning'):
            line, text = text.split('\n', 1)
            warnings += line + '\n'
            if text.startswith('setting ESS_SYSDIR'):
                line, text = text.split('\n', 1)
                warnings.append(line)
        while text and not text.startswith('rosette>'):
            line, text = text.split('\n', 1)
            preamble += line + '\n'
        return warnings, preamble, text


def _mkTempDirMaker(Path, TemporaryDirectory):
    '''Make Path-oriented temp context manager from string-oriented.
    '''
    @contextmanager
    def mkTempDir(**kwargs):
        with TemporaryDirectory(**kwargs) as tmp:
            yield Path(tmp)
    return mkTempDir


def _integration_test(argv, Path, TemporaryDirectory, Popen):
    # ISSUE: get jar from django config?
    jar = '../../rchain/rholang/target/scala-2.12/rholang-assembly-0.1-SNAPSHOT.jar'

    c = Compiler.make(Path(jar), Path, TemporaryDirectory, Popen)

    log.debug('integration test argv: %s text? %s', argv, '--text' in argv)
    if '--text' in argv:
        rbl = c.compile_text(argv[-1])
        log.info("compiled text:\n%s", rbl)
    else:
        out = c.compile_file(Path(argv[-1]))
        log.info("compiled file: %s", out)
        rbl = out.open().read()

    program = Path('../../rchain/rosette/build.out/src/rosette')
    vm = VM.make(program, Popen)
    warnings, preamble, session = vm.run_repl(rbl)
    log.info('vm result:\n%s', session)


if __name__ == '__main__':
    def _script():
        """Use explicit authority for powerful objects.
        """
        from subprocess import Popen
        from sys import argv
        from tempfile import TemporaryDirectory
        from pathlib import Path

        logging.basicConfig(level=logging.DEBUG)
        _integration_test(argv, Path, TemporaryDirectory, Popen)

    _script()
