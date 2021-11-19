import subprocess
import sys
import json

fle = sys.argv[1]

adding = ["<meta name='viewport' content='width=device-width,initial-scale=1,shrink-to-fit=no'>",
          "<link rel='icon' href='https://www.newelectoralcollege.com/favicon.png'>",
          "<link rel='stylesheet' href='https://www.newelectoralcollege.com/static/bootstrap/css/bootstrap.min.css'>",
          "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css'>",
          "<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js'></script>",
          "<script src='https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js'></script>",
          "<script src='https://www.newelectoralcollege.com/static/bootstrap/js/bootstrap.min.js'></script>",
          "<script src='https://www.newelectoralcollege.com/static/js/util.js'></script>"
          ]

try:
    files_file = open("../files.json", "r")
    file_data = json.load(files_file)[fle]
    files_file.close()

except FileNotFoundError:
    print("files.json file not found.")

except KeyError:
    print("\n\n" + fle + " not found in files.json\n\n")

s = subprocess.call("elm make " + fle + " --output=" + file_data["output"])

adding += file_data["header"]
file_data["encoding"] = None if file_data["encoding"] == "None" else file_data["encoding"]

file = open(file_data["output"], "r", encoding=file_data["encoding"])

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
file = open(file_data["output"], "w")
file.write(''.join(text))
file.close()
