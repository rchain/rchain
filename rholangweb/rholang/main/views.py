from subprocess import Popen
from tempfile import TemporaryDirectory
from pathlib import Path

from django import forms
from django.forms import Form
from django.shortcuts import render

from rholang.settings import *
from . import runner

def home(request):
    examples = [
        dict(name=ex.name, src=ex.open().read())
        for ex in Path(EXAMPLES).glob('*.rho')
    ]

    if request.POST:
        compilerForm = CompilerForm(request.POST)
        if compilerForm.is_valid():
            rho = compilerForm.cleaned_data.get('rho')

            try:
                compiler = runner.Compiler.make(
                    Path(COMPILER_JAR), Path, TemporaryDirectory, Popen)
            except runner.ConfigurationError as oops:
                raise  # TODO: HTTP 500

            try:
                rbl = compiler.compile_text(rho)
                compile_error = None
            except runner.UserError as oops:
                rbl = None
                compile_error = str(oops)

            session = None
            run_error = None

            if rbl is not None:
                vm = runner.VM.make(VM_PROGRAM, VM_LIBRARY, Popen)
                _warnings, _preamble, session = vm.run_repl(rbl)
                run_error = None  # TODO
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
