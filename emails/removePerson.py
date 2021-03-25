#!C:\Python27\python

import cgi
import csv
print("Content-Type: text/html; charset=utf-8\n\n")


fs = cgi.FieldStorage().value

if fs == None:
    print("An error occured. Please contact the administrators. (Error 222)")
    assert 1 == 2
else:
    remove = fs

before = []
after = []

file = open("emails.csv", "r")
lines = csv.reader(file)
for first, last, state, email, admin in lines:
    if email == remove:
        after = list(file)
        break
    before.append(",".join([first, last, state, email, admin]))
file.close()

file = open("emails.csv", "w")
file.write("\n".join(before + after))
file.close()

print("You are now unsubscribed!")
