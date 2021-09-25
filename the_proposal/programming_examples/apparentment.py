import json
import os

# Anything that can win seats or votes


class Entity:
    def __init__(self, name):
        self.votes = 0
        self.initial_seats = 0
        self.extra_seats = 0
        self.actual_results = 0
        self.name = name

    def seats(self):
        return self.initial_seats + self.extra_seats

    def divisor(self, quota):
        return (self.votes - (self.initial_seats * quota)) / (self.extra_seats + 1)


class List(Entity):
    def __init__(self, name):
        self.list = []

        super().__init__(name)

    def addList(self, filename):
        file = open("apparentment_data/" + filename, "r")

        for line in file:
            s = line.split(",")

            if self.votes == 0:
                self.votes = int(s[0])
                self.actual_results = int(s[1])

            else:
                self.list.append((s[0], int(s[1])))

        file.close()


class Party(Entity):
    lists: 'list[List]'

    def __init__(self, name):
        self.lists = []

        super().__init__(name)

    def addList(self, name, filename):
        list = List(name)
        list.addList(filename)
        self.lists.append(list)
        self.votes += list.votes
        self.actual_results += list.actual_results


class Apparentment(Entity):
    parties: 'list[Party]'

    def __init__(self, name=""):
        self.parties = []

        super().__init__(name)

    def addParty(self, party: Party):
        self.parties.append(party)
        self.votes += party.votes
        self.actual_results += party.actual_results


def results(competitors: 'list[Apparentment] | list[Party] | list[List]', seats, quota_addition=0):
    total_votes = 0

    for n in competitors:
        total_votes += n.votes

    quota = total_votes // (seats + 1) + quota_addition

    extra_seats = seats

    for n in competitors:
        n.initial_seats = n.votes // quota
        extra_seats -= n.initial_seats

    if extra_seats < 0:
        return results(competitors, seats, quota_addition=quota_addition + 1)

    for e in range(extra_seats):
        n = max(competitors, key=lambda i: i.divisor(quota))
        n.extra_seats += 1

    for n in competitors:
        if n.seats() != 0:
            if type(n) == Apparentment:
                results(n.parties, n.seats())

            elif type(n) == Party:
                results(n.lists, n.seats())

            else:
                pass


def compareResults(apparentments: 'list[Apparentment]'):
    for ap in apparentments:
        for p in ap.parties:
            for l in p.lists:
                difference = l.seats() - l.actual_results

                if difference != 0:
                    print(p.name + " " + str(l.name) + ": " + str(difference))


cantons = list(filter(os.path.isdir, os.scandir("./apparentment_data/")))

national = {}

real_national = {}


def main():

    for canton in cantons:
        canton = canton.path.replace("./apparentment_data/", "")

        try:
            data = json.load(open("./apparentment_data/" +
                                  canton + "/results.json", "r"))

            parties = {}
            apparentments = []

            for party in data["parties"]:
                p = Party(party)
                parties[party] = p

            for party, lists in zip(data["lists"].keys(), data["lists"].values()):
                p = parties[party]

                for list in lists:
                    p.addList(list, "./" + canton + "/" + list + ".txt")

            for apparentment in data["apparentments"]:
                a = Apparentment()

                for party in apparentment:
                    try:
                        a.addParty(parties[party])
                    except KeyError:
                        print("The party " + party +
                              " doesn't seem to exist in Canton: " + canton)

                apparentments.append(a)

            for party in parties:
                if not(party in [x for l in data["apparentments"] for x in l]):
                    a = Apparentment()
                    a.addParty(parties[party])
                    apparentments.append(a)

            print()
            print(canton)

            results(apparentments, data["seats"])

            for party in parties:
                pt: Party = parties[party]
                seats = pt.seats()

                if seats > 0 or pt.actual_results > 0:
                    try:
                        national[party] += seats
                        real_national[party] += pt.actual_results
                    except KeyError:
                        national[party] = seats
                        real_national[party] = pt.actual_results

            compareResults(apparentments)
        except Exception as e:
            raise e

    print()
    print("Switzerland")

    for party in national:
        print(party + ": " + str(national[party] - real_national[party]))

    # for party in real_national:
    #     if not(party in national.keys()):
    #         print(party + ": " + str(0 - real_national[party]))


if __name__ == "__main__":
    main()
