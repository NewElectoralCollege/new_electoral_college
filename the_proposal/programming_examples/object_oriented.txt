#include <vector>
#include <string>

using namespace std;

class Party {
    public:
        const string name;
        const int votes;
        int seats;
        vector<string> candidates;

        int getRemainder(int quota) {
            return votes - (seats * quota);
        }

        Party(int votes);

        static vector<Party> list;

        // In addition to the necessary variables, here are a couple extra

        float percent_female;
        bool getsExtraSeat;
        int wasted_vote;
};