import subprocess

from django import forms
from django.forms import Form
from django.shortcuts import render, get_object_or_404


def home(request):
    if request.POST:
        compilerForm = CompilerForm(request.POST)
        if compilerForm.is_valid():
            input = compilerForm.cleaned_data.get('rho')
            with open('test.rho', 'w') as f:
                f.write("%s\n" % str(input))
            sbt_output = subprocess.check_output("bash sbt.sh", shell=True)
            rbl_output = subprocess.check_output("bash rosette.sh", shell=True)
    else:
        compilerForm = CompilerForm()
        sbt_output = "Please enter a valid Rholang program"
        rbl_output = "Please enter a valid Rholang program"
    return render(request, "index.html", {"form": compilerForm, "sbt_output": sbt_output, "rbl_output": rbl_output})

class CompilerForm(Form):
    rho = forms.CharField(widget=forms.Textarea)
