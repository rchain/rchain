import subprocess
from subprocess import run
from tempfile import TemporaryDirectory
from pathlib import Path

from django import forms
from django.forms import Form
from django.shortcuts import render

from . import runner

# TODO: get jar from django config
JAR = 'rchain/rholang/target/scala-2.12/rholang-assembly-0.1-SNAPSHOT.jar'


def home(request):
    if request.POST:
        compilerForm = CompilerForm(request.POST)
        if compilerForm.is_valid():
            rho = compilerForm.cleaned_data.get('rho')

            try:
                compiler = runner.Compiler.make(
                    Path(JAR), Path, TemporaryDirectory, run)
            except runner.ConfigurationError as oops:
                raise  # TODO: HTTP 500

            try:
                compile_output = compiler.compile_text(rho)
                compile_error = None
            except runner.UserError as oops:
                compile_output = None
                compile_error = str(oops)

            run_output = None
            run_error = None

            if compile_output is not None:
                with open('test.rbl', 'w') as f:
                    f.write("%s\n" % str(compile_output))
                run_output = subprocess.check_output(
                    "bash rosette.sh || true", stderr=subprocess.STDOUT, shell=True)
                run_error = None  # TODO
    else:
        compilerForm = CompilerForm()
        compile_output = "Compiler standing by..."
        compile_error = None
        run_output = "VM standing by..."
        run_error = None
    return render(request, "index.html", {
        "form": compilerForm,
        "sbt_output": compile_output,
        "compile_error": compile_error,  # TODO: update template
        "rbl_output": run_output,
        "run_error": run_error,
    })


class CompilerForm(Form):
    rho = forms.CharField(widget=forms.Textarea)
