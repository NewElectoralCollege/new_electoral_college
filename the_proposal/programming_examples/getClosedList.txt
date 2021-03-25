/*
list.txt:
Bob
Steve
Alex
Theo
Michelle
Peter
Alan
David
Rich
Jack

The candidates from this party are:
Bob
Steve
Alex
Theo
*/

#include <iostream>
#include <fstream>
#include <string>

using namespace std;

void getList(int seats, int total_votes) {
    string text;
    ifstream PartyList("list.txt");
    int elected = 0;

    while (getline(PartyList, text) && elected < seats) {
        cout << text << "\n";
        elected++;
    }

    PartyList.close();
}

int main(void) {
    getList(4, 26228);
    return 1;
}