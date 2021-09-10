# Standard

```json
{
    "parties": [
        {
            "name": "Party A",
            "seats": 4,    
            "votes": 4100,      
            "extra_votes": 60,      
            "extra_seat": false
        },
        {
            "name": "Party B",
            "seats": 4,
            "votes": 4000,
            "extra_votes": 970,
            "extra_seat": true
        },
        {
            "name": "Party C",
            "seats": 2,
            "votes": 2000,
            "extra_votes": 990,
            "extra_seat": true
        }
    ],
    "stats": {
        "name": "Seat Allocation",
        "total_seats": 10,
        "total_votes": 10100,     
        "gallagher_index": 0.52391    
    }
}
```

# Note on New York 2016

Because of a caveat in the proposal, the Independence Party of New York would be awarded an elector for Gary Johnson (rather than the Libertarian Party). To avoid confusion, this model shows the Libertarians winning that elector instead, so that it is associated with Johnson.