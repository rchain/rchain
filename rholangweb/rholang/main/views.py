import subprocess
from django.shortcuts import render, get_object_or_404


def home(request):
    input = ""
    if request.GET.get("input"):
        input = request.GET.get("input")
    else:
        input = "(print 'error)"


    with open('test.rho', 'w') as f:
        f.write("%s\n" % str(input))

    output = subprocess.check_output("bash sbt.sh", shell=True)

    #import ipdb; ipdb.set_trace()
    #base_url = "http://127.0.0.1:8047/api/text/"
    #data = req.post(base_url, data={"txt": self.text}).json()
    #sentiment = data["result"]["sentiment"]
    #if sentiment=="Positive":
    #  return 1
    #elif sentiment=="Negative":
    #  return -1
    #else:
    #  return 0 # Neutral
    
    return render(request, 'index.html', {"output": output})
