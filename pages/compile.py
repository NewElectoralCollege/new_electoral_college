import subprocess
import sys

fle = sys.argv[1]

output = "../" + fle.replace(".elm", ".html").lower()
adding = ["<meta name='viewport' content='width=device-width,initial-scale=1,shrink-to-fit=no'>",
          "<link rel='stylesheet' href='static/bootstrap/css/bootstrap.min.css'>",
          "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css'>",
          "<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js'></script>",
          "<script src='https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js'></script>",
          "<script src='static/bootstrap/js/bootstrap.min.js'></script>",
          "<script src='static/js/util.js'></script>"
          "<link rel='stylesheet' href='static/sass/" +
          fle.replace(".elm", ".css").lower() + "'>"
          ]

mathjax = '<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script><script type="text/javascript" id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"></script>'
encoding = None

if fle == "Map.elm":
    adding.append(
        "<script src='static/js/map.js'></script>")

elif fle == "ReadMore.elm":
    adding.append(
        "<script src='static/js/read_more_lists.js'></script>")
    adding.append(mathjax)

elif fle == "StateResults.elm":
    adding.append(
        "<script src='static/js/state.js'></script>")
    adding.append(mathjax)

elif fle == "Endorsements.elm":
    adding.append(
        "<script async src='https://platform.twitter.com/widgets.js'></script>")
    adding.append(
        "<script async defer src='https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.2'></script>")
    adding.append(
        "<script src='static/js/endorsements.js'></script>")

if fle == "PageNotFound.elm":
    output = "../404.html"

elif fle == "NotAuthorized.elm":
    output = "../403.html"

elif fle == "Donate.elm":
    adding.append("<script src='https://js.stripe.com/v3/'></script>")
    adding.append(
        "<script src='static/js/donate.js'></script>")

elif fle == "Unsubscribe.elm":
    adding.append("<script src='static/js/unsubscribe.js'></script>")

elif fle == "DonateFAQ.elm":
    encoding = "utf-8"

s = subprocess.call("elm make " + fle + " --output=" + output)

file = open(output, "r", encoding=encoding)

text = file.readlines()
text = text[:5] + adding + text[6:]

if fle == "Map.elm":
    text = text[:-17] + ["port();"] + text[-16:]

elif fle == "CountMeIn.elm":
    text = text[:-16] + ["validate_forms();}"] + text[-15:]

elif fle == "StateResults.elm" or fle == "ReadMore.elm" or fle == "Unsubscribe.elm":
    text = text[:-17] + ["init();"] + text[-16:]

elif fle == "PageNotFound.elm" or fle == "NotAuthorized.elm":
    text = text[:-16] + ["includeHTML();}"] + text[-15:]

file.close()
file = open(output, "w")
file.write(''.join(text))
file.close()
