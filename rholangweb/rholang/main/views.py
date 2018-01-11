from pathlib import Path
from resource import setrlimit
from subprocess import Popen
from tempfile import TemporaryDirectory

from django import forms
from django.forms import Form
from django.shortcuts import render

import rholang.settings as cfg
from .runner import Compiler, VM, ConfigurationError, UserError


def home(request):
    examples = [
        dict(name=ex.name, src=ex.open().read())
        for ex in Path(cfg.EXAMPLES).glob('*.rho')
    ]

    if request.POST:
        compilerForm = CompilerForm(request.POST)
        if compilerForm.is_valid():
            rho = compilerForm.cleaned_data.get('rho')

            try:
                runj = Compiler.jar_runner(Path(cfg.COMPILER_JAR), cfg.TIMEOUT,
                                           Popen)
                compiler = Compiler(runj)
            except ConfigurationError as oops:
                raise  # TODO: HTTP 500

            try:
                with TemporaryDirectory(prefix='rholang') as tmp:
                    rbl = compiler.compile_text(rho, work=Path(tmp))
                compile_error = None
            except UserError as oops:
                rbl = None
                compile_error = str(oops)

            session = None
            run_error = None

            if rbl is not None:
                vm = VM(Path(cfg.VM_LIBRARY),
                        VM.program_runner(Path(cfg.VM_PROGRAM),
                                          cfg.TIMEOUT, cfg.STACKLIMIT,
                                          setrlimit, Popen))
                try:
                    _warnings, _preamble, session = vm.run_repl(rbl)
                    run_error = None
                except UserError as oops:
                    run_error = oops.args[0]
                    session = None
    else:
        compilerForm = CompilerForm()
        rbl = "Compiler standing by..."
        compile_error = None
        session = "VM standing by..."
        run_error = None
    return render(request, "index.html", {
        "form": compilerForm,
        "examples": examples,
        "rbl_code": rbl or '',
        "compile_error": compile_error or '',
        "repl_session": session or '',
        "run_error": run_error or '',
    })


class CompilerForm(Form):
    rho = forms.CharField(widget=forms.Textarea)
