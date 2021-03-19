#!C:\Python27\python

print("Content-Type: text/html; charset=utf-8\n\n")

import os
import json
import cgi

year = cgi.FieldStorage().getvalue("year")

output = {}

directory = "/".join(__file__.split("/")[:-3]) + "/data/" + year + "/"
for state in os.listdir(directory):
    file = open(directory + state, "r")
    state = state.split(".")[0].replace(" Of ", " of ")
    output[state] = ""

    for line in file:
        output[state] += line.strip()

    output[state] = output[state]

    file.close()

print(json.dumps(output))