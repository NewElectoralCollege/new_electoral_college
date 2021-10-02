module Ticket exposing (Ticket, nominee, realElectors)

import List.Extra exposing (find)
import Maybe exposing (map)
import Party exposing (PartyName(..))


type alias Ticket =
    { year : Int
    , party : PartyName
    , nominee : String
    , real_electors : Int
    }


correctTicket : Int -> PartyName -> Ticket -> Bool
correctTicket year party t =
    t.year == year && t.party == party


ticket : Int -> PartyName -> Maybe Ticket
ticket year party =
    find (correctTicket year party) tickets


nominee : Int -> PartyName -> Maybe String
nominee year party =
    map .nominee <| ticket year party


realElectors : Int -> PartyName -> Maybe Int
realElectors year party =
    map .real_electors <| ticket year party



--This may not be the cleanest way of storing data, but it is better than having to make a bunch of GET calls.


tickets : List Ticket
tickets =
    [ Ticket 2020 Democratic "Joe Biden" 306
    , Ticket 2020 Republican "Donald Trump" 232
    , Ticket 2020 Libertarian "Jo Jorgensen" 0
    , Ticket 2016 Democratic "Hillary Clinton" 232
    , Ticket 2016 Republican "Donald Trump" 306
    , Ticket 2012 Democratic "Barack Obama" 332
    , Ticket 2012 Republican "Mitt Romney" 206
    , Ticket 2008 Democratic "Barack Obama" 365
    , Ticket 2008 Republican "John McCain" 173
    , Ticket 2004 Democratic "John Kerry" 252
    , Ticket 2004 Republican "George W. Bush" 286
    , Ticket 2000 Democratic "Al Gore" 267
    , Ticket 2000 Republican "George W. Bush" 271
    , Ticket 1996 Democratic "Bill Clinton" 379
    , Ticket 1996 Republican "Bob Dole" 159
    , Ticket 1992 Democratic "Bill Clinton" 370
    , Ticket 1992 Republican "George H. W. Bush" 168
    , Ticket 1988 Democratic "Michael Dukakis" 112
    , Ticket 1988 Republican "George H. W. Bush" 426
    , Ticket 1984 Democratic "Walter Mondale" 13
    , Ticket 1984 Republican "Ronald Reagan" 525
    , Ticket 1980 Democratic "Jimmy Carter" 49
    , Ticket 1980 Republican "Ronald Reagan" 489
    , Ticket 1976 Democratic "Jimmy Carter" 297
    , Ticket 1976 Republican "Gerald Ford" 241
    , Ticket 2016 Libertarian "Gary Johnson" 0
    , Ticket 2016 Green "Jill Stein" 0
    , Ticket 2016 (Independent (Just "McMullin, Evan")) "Evan McMullin" 0
    , Ticket 2012 Libertarian "Gary Johnson" 0
    , Ticket 2008 PeaceAndFreedom "Ralph Nader" 0
    , Ticket 2000 Green "Ralph Nader" 0
    , Ticket 1996 Green "Ralph Nader" 0
    , Ticket 1996 Reform "Ross Perot" 0
    , Ticket 1992 (Independent (Just "Perot, Ross")) "Ross Perot" 0
    , Ticket 1980 (Independent (Just "Anderson, John B.")) "John B. Anderson" 0
    , Ticket 1980 Libertarian "Ed Clark" 0
    , Ticket 1976 (Independent (Just "McCarthy, Eugene")) "Eugene McCarthy" 0
    , Ticket 2004 (Independent (Just "Nader, Ralph")) "Ralph Nader" 0
    ]
