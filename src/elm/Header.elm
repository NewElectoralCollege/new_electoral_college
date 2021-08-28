port module Header exposing (main)

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
import Html.Attributes exposing (attribute, href, id, name, rel, src, title, type_)



-- Required


needsMathJax : List String
needsMathJax =
    [ "More Reading", "Viewing State" ]



-- Type definitions


type alias Model =
    String



-- Required functions


init : String -> ( Model, Cmd msg )
init page =
    ( page, title <| "The New Electoral College - " ++ page )


view : Model -> Html msg
view model =
    node
        "head"
        []
        [ node
            "meta"
            [ attribute "charset" "utf-8" ]
            []
        , node
            "meta"
            [ name "viewport", attribute "content" "width=device-width,initial-scale=1,shrink-to-fit=no" ]
            []
        , node
            "link"
            [ rel "icon", href "/new_electoral_college/src/img/icon.png" ]
            []
        , node
            "link"
            [ rel "stylesheet", href "https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css" ]
            []
        , node
            "link"
            [ rel "stylesheet", href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" ]
            []
        , node
            "script"
            [ src "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js" ]
            []
        , node
            "script"
            [ src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js" ]
            []
        , node
            "script"
            [ src "https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js" ]
            []
        , if List.member model needsMathJax then
            node
                "script"
                [ id "MathJax-script"
                , type_ "text/javascript"
                , src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"
                ]
                []

          else
            text ""
        , if compare model "404 Page Not Found" == EQ then
            node
                "link"
                [ rel "stylesheet"
                , href "/new_electoral_college/src/sass/404.css"
                ]
                []

          else
            text ""
        , node
            "link"
            [ rel "stylesheet"
            , href "/new_electoral_college/src/sass/style.css"
            ]
            []
        ]


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


main : Program String Model msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port title : String -> Cmd a
