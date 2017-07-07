from django.shortcuts import render, get_object_or_404


def home(request):
    input = ""
    if request.GET.get("input"):
        input = request.GET.get("input")
    else:
        input = "(print 'error)"

    import ipdb; ipdb.set_trace()
    #base_url = "http://127.0.0.1:8047/api/text/"
    #data = req.post(base_url, data={"txt": self.text}).json()
    #sentiment = data["result"]["sentiment"]
    #if sentiment=="Positive":
    #  return 1
    #elif sentiment=="Negative":
    #  return -1
    #else:
    #  return 0 # Neutral
    
    output = 5 #// ...
    return render(request, 'index.html', {"output": output})
