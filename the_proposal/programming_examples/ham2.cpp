// Don't define main()
#define LIB

// C++ implementation above
#include "c++.cpp"
#include <algorithm>

typedef struct
{
    Party *party;
    double divisor;
} divisor;

inline bool compareDivisors(divisor *a, divisor *b)
{
    return a->divisor < b->divisor;
}

int main()
{
    makeList();

    int seats, n;
    std::vector<divisor *> ds;
    divisor *d;

    seats = 500;

    for (Party *p : Party::list)
    {
        d = new divisor{p, (double)p->getVotes()};
        ds.push_back(d);
    }

    for (n = 0; n < seats; n++)
    {
        auto i = std::max_element(ds.begin(), ds.end(), compareDivisors);

        (*i)->party->addSeats(1);
        (*i)->divisor = (*i)->party->getVotes() / ((*i)->party->getSeats() + 1);
    }

    for (Party *p : Party::list)
    {
        std::cout << p->results();
    }
}