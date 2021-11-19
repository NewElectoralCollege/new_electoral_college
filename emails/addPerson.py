#!/usr/bin/python

import csv
import cgi

from emails import send

print("Content-Type: text/html; charset=utf-8\n\n")

form = cgi.FieldStorage()

form_result = ["False"]

for term in ["first-name", "last-name", "state", "email"]:
    form_result.insert(len(form_result)-1, form.getvalue(term))

with open("emails.csv", "r") as file:
    lines = csv.reader(file)

    for first, last, state, email, admin in lines:
        if email == form_result[3]:
            exit(-1)
    

file.close()

file = open("emails.csv", "a")
file.write("\n" + ",".join(form_result))
file.close()

send("Thank you for Signing Up for the Newsletter!", "welcome_email.html", form_result)
