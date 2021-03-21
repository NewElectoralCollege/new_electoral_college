port module Header exposing (..)

{-

This page requires the use of <script> tags. These are imported in elm. The compiler does not allow you to import scripts like that,
so the Header_compiled.js file must be edited to allow for this:

// XSS ATTACK VECTOR CHECKS

function _VirtualDom_noScript(tag)
{
    return tag == 'script' ? 'p' : tag;
}

becomes

function _VirtualDom_noScript(tag)
{
    return tag == 'scrip' ? 'p' : tag;
}

-}

import Browser exposing (element)
import Html exposing (Html, node, text)
import Html.Attributes exposing (attribute, name, title, href, rel, src, attribute, id)
import List exposing (member)

-- Required

needsFontAwesome : List String
needsFontAwesome =
    ["404 Page Not Found", "Main", "Viewing Results", "Viewing State"]

needsMathJax : List String
needsMathJax =
    ["More Reading", "Viewing State"]

-- Type definitions

type Msg
    = CreateHtml

type alias Model
    = String

-- Required functions

init : String -> (Model, Cmd Msg)
init page =
    (page, title <| "The New Electoral College - " ++ page)

view : Model -> Html Msg
view model =
    node
        "head"
        [ ]
        [ node
            "meta"
            [ attribute "charset" "utf-8" ]
            [ ]
        , node
            "meta"
            [ name "viewport", attribute "content" "width=device-width,initial-scale=1,shrink-to-fit=no" ]
            [ ]
        , node
            "link"
            [ rel "icon", href "src/img/icon.png" ]
            [ ]
        , node
            "link"
            [ rel "stylesheet", href "https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css" ]
            [ ]
        , (
            if member model needsFontAwesome then
                node "link" [ rel "stylesheet", href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" ] []
            else
                text ""
        )
        , node
            "script"
            [ src "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js" ]
            [ ]
        , node
            "script"
            [ src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js" ]
            [ ]
        , node
            "script"
            [ src "https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js" ]
            [ ]
        , (
            if member model needsMathJax then
                node
                    "script"
                    [ id "MathJax-script"
                    , src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"
                    ]
                    []
            else
                text ""
            )
        , (
            if compare model "404 Page Not Found" == EQ then
                node
                    "link"
                    [ rel "stylesheet"
                    , href "src/sass/404.css"
                    ]
                    []
            else
                text ""
            )
        , node
            "link"
            [ rel "stylesheet"
            , href "src/sass/style.css" ]
            [ ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

main : Program String Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

port title : String -> Cmd a