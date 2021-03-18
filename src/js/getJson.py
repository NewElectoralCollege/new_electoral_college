#!C:\Python27\python

print("Content-Type: text/html; charset=utf-8\n\n")

import os
import json
import sys
from flask import Flask, session, redirect, url_for, request

app = Flask(__name__)

@app.route('/')
def summary():
    sys.argv.append("2020")

    year = sys.argv[1]

    output = {}

    directory = "/".join(__file__.split("\\")[:-3]) + "/data/" + year + "/"
    for state in os.listdir(directory):
        file = open(directory + state, "r")
        state = state.split(".")[0]
        output[state] = ""

        for line in file:
            output[state] += line.strip()

        output[state] = output[state]

        file.close()

        return app.response_class(response=json.dumps(output), mimetype="application/json")