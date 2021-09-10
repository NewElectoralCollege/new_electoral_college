#!C:\Python27\python

import cgi
import json
import os
print("Content-Type: text/html; charset=utf-8\n\n")


year = cgi.FieldStorage().getvalue("year")

output = {}

directory = "../data/" + year + "/"
for state in os.listdir(directory):
    file = open(directory + state, "r")
    state = state.split(".")[0].replace(" Of ", " of ")
    output[state] = ""

    for line in file:
        output[state] += line.strip()

    output[state] = output[state]

    file.close()

print(json.dumps(output))
