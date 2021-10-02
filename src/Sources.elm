module Sources exposing (getCitation)

import Html exposing (Html, a, p, text)
import Html.Attributes exposing (href, style, target)


getCitationHelper : Int -> ( String, String )
getCitationHelper year =
    if year < 2020 && year > 1996 then
        ( "Federal Election Commission (FEC)", "https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/" )

    else
        ( "Massachusetts Institute of Technology (MIT) Election Lab", "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX" )


getCitation : Int -> Html msg
getCitation year =
    let
        ( name, url ) =
            getCitationHelper year
    in
    p [ style "float" "right", style "text-align" "right" ]
        [ text "Data Source: "
        , a
            [ target "_blank"
            , href url
            ]
            [ text name ]
        ]
