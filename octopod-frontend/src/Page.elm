module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Common exposing (bClass, divClass)


type Page
    = Initialization
    | Deployments
    | Deployment


view : String -> Page -> { title : String, content : List (Html msg) } -> Document msg
view projectName page { title, content } =
    { title = title
    , body = viewHeader projectName :: content
    }


viewHeader : String -> Html msg
viewHeader projectName =
    Html.header
        [ class "header"
        ]
        [ divClass "header__wrap container"
            [ bClass "header__logo"
                [ text "Deployment Manager" ]
            , divClass "header__project"
                [ text projectName ]
            ]
        ]