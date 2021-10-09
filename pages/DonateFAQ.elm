module DonateFAQ exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, div, text)
import Html.Attributes exposing (class, href, type_)
import Markdown exposing (toHtmlWith)


options : Markdown.Options
options =
    { githubFlavored = Just { tables = False, breaks = False }
    , defaultHighlighting = Nothing
    , sanitize = False
    , smartypants = False
    }


body : Html Never
body =
    div [ class "container" ]
        [ toHtmlWith options [] markdown
        , a [ type_ "button", class "btn btn-primary", href "/new_electoral_college/donate.html" ] [ text "Donate" ]
        ]


markdown : String
markdown =
    """
# Donation FAQs
    
## Who can contribute?

- Individuals
- Green Card holders
- [Partnerships and Limited Liability Companies (LLCs)](https://www.fec.gov/help-candidates-and-committees/candidate-taking-receipts/partnership-llc-contributions/)

[Read More](https://www.fec.gov/help-candidates-and-committees/taking-receipts-pac/who-can-and-cant-contribute-nonconnected-pac/)

## Who can't contribute?

- Foreign Nationals
- Foreign Entities
- Federal Contractors
- National Banks
- Federally Chartered Corporations

[Read More](https://www.fec.gov/help-candidates-and-committees/taking-receipts-pac/who-can-and-cant-contribute-nonconnected-pac/)

## Is there a maximum donation size?

Account A has no maximum donation size. Account B has a maximum donation amount of $5,000 a year. The donation you make is split in half, with one half going to Account A and the other half to Account B. If the limit for Account B is hit, the remainder goes to Account A.

Cash Donations are limited to $100 or less.

Anonymous Donations are limited to $50 or less.

[Read More](https://www.fec.gov/help-candidates-and-committees/taking-receipts-pac/contribution-limits-nonconnected-pacs/)

## Is the $5,000 limit a year for a full year or a calender year?

A calender year. On December 31 your limit will reset.

## Can I contribute anonymously?

Yes. Simply click the check mark on the donation page. You will be limited to $50.

## Is there a contribution count limit?

No. You may contribute as often as you want.

## Can my spouse contribute independently of the $5,000 limit?

Yes, the limit is separate for each individual, even if only one spouse has an income.

## Can people under the age of 18 contribute?

Yes, they can. However, the following must apply:

- The juvenile knowingly and voluntarily makes the contribution; and
- The contribution is owned and controlled by the juvenile.

People under the age of 18 do not need to ask permission from a parent or guardian to make the contribution.

The contribution can't be made using funds given as a gift to the juvenile for the purpose of making the contribution.

## Do you need to have American Citizenship/Nationality to contribute?

Only American Nationals may contribute. This includes citizens of D.C., and the Territories. Residents of American Samoa are American Nationals, so they may contribute as well. 

## Can I give a contribution in another's name?

No. You also can't give someone funds to contribute to the campaign.

## Do you take Bitcoin?

Currently, we do not accept bitcoin donations. If we recive enough requests, we are open to taking bitcoin in the future.

## Are there public officials that you won't contribute to?

On exceedingly rare occasions, we will not contribute to a figure that endorses our proposal if we believe that their values pose an active danger to democracy. We do not specificly define these individuals, apart from the eight senators that voted to overturn the Electoral College results in 2021.

"""


main : Program () () Never
main =
    document
        { init = always ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        , view =
            always
                { title = "The New Electoral College - Donation FAQs"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], body, footer ]
                }
        }
