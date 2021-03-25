import csv

states = {}
admins = 0
total = 0

with open("/".join(__file__.split("\\")[:-1]) + "/emails.csv", "r") as file:
    lines = csv.reader(file)
    for first, last, state, email, admin in lines:
        if state in list(states.keys()):
            states[state] += 1
        else:
            states[state] = 1
        admins += 1
        total += 1

    file.close()

print("BY STATE: ")
for state in states:
    print(state + ": " + str(states[state]))
print()

print("Total Entries: " + str(total))
print("Admins: " + str(admins))
