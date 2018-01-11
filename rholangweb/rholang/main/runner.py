"""runner -- compile and run rholang programs
"""

from subprocess import PIPE, SubprocessError, CalledProcessError
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


class RunError(UserError):
    pass


class TimeoutError(UserError):
    pass


class Compiler(object):

    def __init__(self, run_jar):
        self.__run_jar = run_jar

    @classmethod
    def make(cls, jarRd, Popen):
        '''Construct from python stdlib powers.
        '''
        return cls(cls.jar_runner(jarRd, Popen))

    @classmethod
    def jar_runner(self, jarRd, Popen):
        try:
            proc = Popen(['java', '-version'], stderr=PIPE)
            _, err = proc.communicate()
            if proc.returncode != 0:
                raise JavaRequired(err)
            log.info('java version:\n%s', err.decode('us-ascii'))
        except OSError as oops:
            raise JavaRequired() from oops

        try:
            jarRd.open()
        except OSError as oops:
            raise ConfigurationError() from oops

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

    def compile_text(self, text, work,
                     filename='playground.rho'):
        input = work / filename
        with input.open('w') as fp:
            fp.write(text)
        output = self.compile_file(input)
        return output.open().read()


class VM(object):
    def __init__(self, runVM):
        self.__runVM = runVM

    @classmethod
    def make(cls, program, library, Popen, Timer):
        # Adapted from http://www.ostricher.com/2015/01/python-subprocess-with-timeout/  # noqa
        def run_command_with_timeout(cmd, input, timeout_sec):
            proc = Popen(map(str, cmd), stdin=PIPE, stdout=PIPE, stderr=PIPE)
            timer = Timer(timeout_sec, proc.kill)
            timer.start()
            return_value = proc.communicate(input.encode('utf-8'))
            if timer.is_alive():
                timer.cancel()
                return return_value
            raise TimeoutError(
                'Process #%d killed after %d seconds.'
                ' Try checking for infinite loops.' % (proc.pid, timeout_sec))

        def runVM(rbl):
            cmd = [program,
                   "-boot", (library / "boot.rbl").relative_to(program.parent)]
            # ISSUE: timout setting?
            return run_command_with_timeout(cmd, rbl, 1)
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


def _integration_test(argv, Path, TemporaryDirectory, Popen):
    import rholang.settings as cfg  # ISSUE: PYTHONPATH?

    c = Compiler.make(Path(cfg.COMPILER_JAR), Popen)

    log.debug('integration test argv: %s text? %s', argv, '--text' in argv)
    if '--text' in argv:
        with TemporaryDirectory(prefix='rholang') as tmp:
            rbl = c.compile_text(argv[-1], Path(tmp))
        log.info("compiled text:\n%s", rbl)
    else:
        out = c.compile_file(Path(argv[-1]))
        log.info("compiled file: %s", out)
        rbl = out.open().read()

    program = Path(cfg.VM_PROGRAM)
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
