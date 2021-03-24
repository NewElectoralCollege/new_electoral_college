#include <iostream>
#include <algorithm>
#include <vector>
#include <math.h>

using namespace std;

static int total_votes;
static int quota;

class Party {
    public:
        int number, votes, seats;

        Party(int votes);

        static vector<Party> list;

        struct ScoreOperator {
            bool operator()(const Party &a, const Party &b) const {
                return
                    a.votes - (a.seats * quota) <
                    b.votes - (b.seats * quota);
            }
        };
};

vector<Party> Party::list;

Party::Party(int votes) {
    this->number = Party::list.size() + 1;
    this->votes = votes;
    this->seats = 0;
    total_votes += votes;
}

int main() {
    Party::list.push_back(Party(400000));
    Party::list.push_back(Party(250000));
    Party::list.push_back(Party(100000));
    Party::list.push_back(Party(73000));
    Party::list.push_back(Party(5000));

    int seats, awarded, awarding, i;
    seats = 5;
    quota = floor(total_votes / seats);

    for (Party &p : Party::list) {
        awarding = floor(p.votes / quota);
        awarded += awarding;
        p.seats += awarding;
    }

    for (; awarded < seats; awarded++) {
        int n = distance(Party::list.begin(), max_element(
                                                Party::list.begin(), 
                                                Party::list.end(), 
                                                Party::ScoreOperator()));
        Party &p = Party::list.at(n);
        p.seats += 1;
    }

    for (Party p : Party::list) {
        cout << p.number << ", " << p.seats << " seats\n";
    }
}