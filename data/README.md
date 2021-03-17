# Standard

```json
{
    "parties": [
        {
            "name": "Party A",
            "seats": 4,              // Seats
            "votes": 4100,           // Votes
            "extra_votes": 60,       // Votes after quota division
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
        "total_seats": 10,             // Total Seats
        "total_votes": 10100,          // Total Votes
        "gallagher_index": 0.52391     // Gallagher Index 
    }
}
```