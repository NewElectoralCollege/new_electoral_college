import csv
import json

canton = "bern"
data = {"parties": [], "lists": {}, "apparentments": [],  "seats": 0}
votes_l = {}
results = {}
candidates = {}

data["apparentments"] = [
    ["PdA/Sol.", "GPS", "SP"], ["GLP", "EVP", "BDP", "CVP"], ["SD", "EDU"]
]

with open("./data.csv", "r") as file:
    lines = csv.reader(file)

    for list, party, abbvr, mandates, votes, share, cn in lines:
        if cn != canton:
            continue

        list = list.replace("\u00ef\u00bb\u00bf", "")

        if abbvr == "\u00c3\u0153brige":
            abbvr = "Other - " + list

        if not(abbvr in data["parties"]):
            data["parties"].append(abbvr)
            data["lists"][abbvr] = []

        data["lists"][abbvr].append(list)
        candidates[list] = []
        votes_l[list] = int(votes)
        data["seats"] += int(mandates)

        try:
            results[list] = int(mandates)
        except ValueError:
            results[list] = 0

    file.close()

with open("./" + canton + "/results.json", "w") as file:
    file.write(json.dumps(data))

    file.close()

with open("./candidates.csv", "r") as file:
    lines = csv.reader(file)

    for n, name, elected, votes, gender, status, party, list, cn in lines:
        if cn != canton:
            continue

        candidates[list].append((name[:-7], int(votes)))

    file.close()

for n, list in zip(candidates.keys(), candidates.values()):
    file = open("./" + canton + "/" + str(n) + ".txt", "w")

    file.write(str(votes_l[n]) + "," + str(results[n]) + "\n")

    for candidate in list:
        file.write(candidate[0] + "," + str(candidate[1]) + "\n")

    file.close()
