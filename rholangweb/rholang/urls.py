"""tindit URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/1.8/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  url(r'^$', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  url(r'^$', Home.as_view(), name='home')
Including another URLconf
    1. Add an import:  from blog import urls as blog_urls
    2. Add a URL to urlpatterns:  url(r'^blog/', include(blog_urls))
"""

from pathlib import Path
from resource import setrlimit
from subprocess import Popen
import logging

from django.conf.urls import include, url
from django.contrib import admin

import rholang.settings as cfg
from rholang.main.views import Playground
from rholang.main.runner import Compiler, VM

log = logging.getLogger(__name__)

saveDir = Path(cfg.SAVED_CONTRACTS)
if not saveDir.exists():
    log.warn('SAVED_CONTRACTS dir %s does not exist.', saveDir)

runj = Compiler.jar_runner(
    Path(cfg.COMPILER_JAR), cfg.TIMEOUT, Popen)
runvm = VM.program_runner(Path(cfg.VM_PROGRAM),
                          cfg.TIMEOUT, cfg.STACKLIMIT,
                          setrlimit, Popen)
play = Playground(Compiler(runj),
                  VM(Path(cfg.VM_LIBRARY), runvm),
                  Path(cfg.EXAMPLES), saveDir)

urlpatterns = [
    url(r'^admin/', include(admin.site.urls)),
    url(r'^$', play.home, name="home"),
    url(r'^save$', play.save, name="save"),
]
