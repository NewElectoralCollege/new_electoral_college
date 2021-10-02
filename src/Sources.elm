module Sources exposing (getCitation)

import Html exposing (Html, a, p, text)
import Html.Attributes exposing (class, href, target)


getSource : Int -> ( String, String )
getSource year =
    if year < 2020 && year > 1996 then
        ( "Federal Election Commission (FEC)", "https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/" )

    else
        ( "Massachusetts Institute of Technology (MIT) Election Lab", "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX" )


getCitationHelper : ( String, String ) -> Html msg
getCitationHelper ( name, url ) =
    p [ class "citation" ]
        [ text "Data Source: "
        , a
            [ target "_blank"
            , href url
            ]
            [ text name ]
        ]


getCitation : Int -> Html msg
getCitation =
    getCitationHelper << getSource
