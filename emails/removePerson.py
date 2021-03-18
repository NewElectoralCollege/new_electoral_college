import csv
from sys import argv

remove = ""

if len(argv) > 1:
    remove = argv[1]

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

if len(after) > 0:
    file = open("emails.csv", "w")
    for n in before + after:
        file.write(n + ("" if n == after[-1] else "\n"))
    file.close()
