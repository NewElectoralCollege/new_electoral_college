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
    return a->divisor > b->divisor;
}

int main()
{
    makeList();

    int seats, n;
    std::vector<divisor *> ds;
    divisor *d;
    double dv;

    seats = 500;

    for (Party *p : Party::list)
    {
        for (n = 1; n <= seats; n++)
        {
            dv = p->getVotes() / (double)n;

            d = new divisor{p, dv};
            ds.push_back(d);
        }
    }

    std::sort(ds.begin(), ds.end(), compareDivisors);

    for (n = 1; n <= seats; n++)
    {
        d = ds[n - 1];
        d->party->addSeats(1);
    }

    for (Party *p : Party::list)
    {
        std::cout << p->results();
    }
}