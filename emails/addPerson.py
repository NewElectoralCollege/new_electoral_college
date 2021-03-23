#!C:\Python27\python

print("Content-Type: text/html; charset=utf-8\n\n")

import cgi
import csv
from emails import send

form = cgi.FieldStorage()

list = ["False"]

for term in ["first-name", "last-name", "state", "email"]:
    list.insert(len(list)-1, form.getvalue(term))

with open("emails.csv", "r") as file:
    lines = csv.reader(file)
    for first, last, state, email, admin in lines:
        if email == list[3]:
            exit(-1)

file.close()

file = open("emails.csv", "a")
file.write("\n" + ",".join(list))
file.close()

send("Thank you for Signing Up for the Newsletter!", "/welcome_email.html", list)