#include <stdio.h>
#include <math.h>

int main() {
    // {#, Votes, Seats}
    int parties[5][3] = {
        {1, 400000, 0},
        {2, 250000, 0},
        {3, 100000, 0},
        {4, 73000, 0},
        {5, 5000, 0}
    };

    int quota, total_votes, seats, awarded, awarding, i;
    total_votes = 828000;
    seats = 5;
    quota = floor(total_votes / seats);

    for (i = 0; i < 5; i++) {
        awarding = floor(parties[i][1] / quota);
        awarded += awarding;
        parties[i][2] += awarding;
    }

    for (; awarded < seats; awarded++) {
        int party = 0;
        for (i = 1; i < 5; i++) {
            int remainder = parties[i][1] - (parties[i][2] * quota);
            if (
                parties[i][1] - (parties[i][2] * quota) > 
                parties[party][1] - (parties[party][2] * quota)
            ) {
                party = i;
            }
        }
        parties[party][2] += 1;
    }

    for (i = 0; i < 5; i++) {
        printf("%i, %i seats\n", parties[i][0], parties[i][2]);
    }
}