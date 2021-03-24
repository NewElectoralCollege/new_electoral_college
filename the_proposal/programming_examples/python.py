parties = {
    1: {'votes': 400000, 'seats': 0},
    2: {'votes': 250000, 'seats': 0},
    3: {'votes': 100000, 'seats': 0},
    4: {'votes': 73000, 'seats': 0},
    5: {'votes': 5000, 'seats': 0}
}

def sortRemainder(val):
    return val['remainder']

total_votes = 828000
seats = 5
quota = total_votes // seats
awarded = 0

for p in parties.values():
    awarding = p['votes'] // quota
    p['seats'] += awarding
    awarded += awarding
    p['remainder'] = p['votes'] - (awarding * quota)

for p in sorted(list(parties.values()), key=sortRemainder, reverse=True)[:seats-awarded]:
    p['seats'] += 1

print(parties)