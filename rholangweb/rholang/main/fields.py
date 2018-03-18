from __future__ import absolute_import

from django import forms
from django.db import models
from .widgets import RhoEditorWidget


class RholangTextFormField(forms.fields.CharField):
    def __init__(self, *args, **kwargs):
        kwargs.update({"widget":RhoEditorWidget()})
        super(RholangTextFormField, self).__init__(*args, **kwargs)
