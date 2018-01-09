"""runner -- compile and run rholang programs
"""

from pathlib import Path
from sys import stderr
from subprocess import PIPE, CalledProcessError
from contextlib import contextmanager
import logging

log = logging.getLogger(__name__)


class ConfigurationError(Exception):
    pass


class JavaRequired(ConfigurationError):
    pass


class UserError(Exception):
    pass


class CompileError(UserError):
    pass


class Compiler(object):

    def __init__(self, run_jar, mkTempDir):
        self.__run_jar = run_jar
        self.__mkTempDir = mkTempDir

    @classmethod
    def make(cls, jarRd, Path, TemporaryDirectory, run):
        '''Construct from python stdlib powers.
        '''
        return cls(cls.jar_runner(run, jarRd),
                   _mkTempDirMaker(Path, TemporaryDirectory))

    @classmethod
    def jar_runner(self, run, jarRd):
        try:
            ok = run(['java', '-version'],
                     stderr=PIPE, check=True)
        except oops:
            raise JavaRequired() from oops

        def run_jar(argv):
            argv = ['java', '-jar', str(jarRd)] + list(argv)
            log.info('running: %s', argv)
            return run(argv,
                       stdout=PIPE, stderr=PIPE, check=True)
        return run_jar

    def compile_file(self, input):
        output = input.with_suffix('.rbl')

        # ISSUE: we assume the compiler produces foo.rbl from foo.rho
        try:
            result = self.__run_jar([str(input)])
        except CalledProcessError as oops:
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


def _mkTempDirMaker(Path, TemporaryDirectory):
    '''Make Path-oriented temp context manager from string-oriented.
    '''
    @contextmanager
    def mkTempDir(**kwargs):
        with TemporaryDirectory(**kwargs) as tmp:
            yield Path(tmp)
    return mkTempDir


def _integration_test(argv, Path, TemporaryDirectory, run):
    # ISSUE: get jar from django config?
    jar = '../../rchain/rholang/target/scala-2.12/rholang-assembly-0.1-SNAPSHOT.jar'

    c = Compiler.make(Path(jar), Path, TemporaryDirectory, run)

    log.debug('integration test argv: %s text? %s', argv, '--text' in argv)
    if '--text' in argv:
        rbl = c.compile_text(argv[-1])
        log.info("compiled text:\n%s", rbl)
    else:
        out = c.compile_file(Path(argv[-1]))
        log.info("compiled file: %s", out)


if __name__ == '__main__':
    def _script():
        """Use explicit authority for powerful objects.
        """
        from subprocess import run
        from sys import argv
        from tempfile import TemporaryDirectory
        from pathlib import Path

        logging.basicConfig(level=logging.DEBUG)
        _integration_test(argv, Path, TemporaryDirectory, run)

    _script()
