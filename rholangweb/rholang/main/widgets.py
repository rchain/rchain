from __future__ import absolute_import


from django import forms
from django import forms
from django.conf import settings
from django.core.exceptions import ImproperlyConfigured
from django.core.serializers.json import DjangoJSONEncoder
from django.template.loader import render_to_string
from django.utils.encoding import force_text
from django.utils.functional import Promise
from django.utils.html import conditional_escape
from django.utils.safestring import mark_safe
from django.utils.translation import get_language
from js_asset import JS, static


class RhoEditorWidget(forms.Textarea):
    class Media:
        js = (
            JS("js/rhoeditor/ace.js",{}),
            JS("js/rhoeditor/ext-language_tools.js",{})
        )
    def __init__(self, *args, **kwargs):
        super(RhoEditorWidget, self).__init__( *args, **kwargs)

    def render(self, name, value, attrs=None, renderer=None):
        return mark_safe(render_to_string(
            "rhowidget.html"
        ))