from pathlib import Path
from tempfile import TemporaryDirectory
import logging

from django import forms
from django.http import HttpResponse
from django.forms import Form
from django.shortcuts import render

from .runner import UserError

log = logging.getLogger(__name__)


class Playground(object):
    def __init__(self, compiler, vm, exampleDir, saveDir):
        self.__compiler = compiler
        self.__vm = vm
        self.__exampleDir = exampleDir
        self.__saveDir = saveDir

    def home(self, request):
        compilerForm = CompilerForm()
        rbl = "Compiler standing by..."
        compile_error = None
        session = "VM standing by..."
        run_error = None
        if request.POST:
            compilerForm = CompilerForm(request.POST)
            if compilerForm.is_valid():
                rho = compilerForm.cleaned_data.get('rho')
                verbose = compilerForm.cleaned_data.get('verbose')
                compile_error, rbl, run_error, session = self.run(rho, verbose)
            else:
                pass  # TODO: messages or something for invalid input

        return render(request, "index.html", {
            "form": compilerForm,
            "examples": exampleMenu([self.__exampleDir, self.__saveDir]),
            "rbl_code": rbl or '',
            "compile_error": compile_error or '',
            "repl_session": session or '',
            "run_error": run_error or '',
        })

    def run(self, rho, verbose):
        try:
            with TemporaryDirectory(prefix='rholang') as tmp:
                rbl = self.__compiler.compile_text(rho, work=Path(tmp))
        except UserError as oops:
            log.error('compile_text: %s', oops)
            return str(oops), None, None, None

        try:
            _warnings, _preamble, session = self.__vm.run_repl(rbl, verbose)
        except UserError as oops:
            log.error('run_repl: %s', oops)
            return None, rbl, oops.args[0], None
        return None, rbl, None, session

    def save(self, request):
        if not request.POST:
            return HttpResponse(status=405)

        saveForm = SaveForm(request.POST)
        if not saveForm.is_valid():
            return HttpResponse(saveForm.errors.as_json(), status=400)

        rho = saveForm.cleaned_data.get('rhoSave')
        filename = saveForm.cleaned_data.get('basename')
        dest = (self.__saveDir / filename).with_suffix('.rho')
        log.info('saving text %s... (%d) to %s',
                 rho[:8], len(rho), filename)
        try:
            with dest.open('w') as f:
                f.write(rho + "\n")
        except IOError as oops:
            log.error('cannot save: %s', oops)
            return HttpResponse('failed to save', status=500)

        return HttpResponse(status=204)


def exampleMenu(paths):
    log.debug('example menu paths: %s', paths)
    return [
        dict(name=ex.name, src=ex.open().read())
        for where in paths
        for ex in where.glob('*.rho')
    ]


class CompilerForm(Form):
    rho = forms.CharField(widget=forms.Textarea)
    verbose = forms.BooleanField(label="Verbose", required=False)


class SaveForm(Form):
    rhoSave = forms.CharField(widget=forms.HiddenInput)
    basename = forms.CharField()
