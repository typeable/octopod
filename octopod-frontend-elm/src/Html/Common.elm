module Html.Common exposing (..)

import Html exposing (Attribute, Html, a, b, button, div, h1, h2, h3, li, span, table, tbody, td, th, thead, tr, ul)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Route exposing (Route)


elClass : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> List (Html msg) -> Html msg
elClass el cls body =
    el [ class cls ] body


divClass : String -> List (Html msg) -> Html msg
divClass =
    elClass div


bClass : String -> List (Html msg) -> Html msg
bClass =
    elClass b


ulClass : String -> List (Html msg) -> Html msg
ulClass =
    elClass ul


liClass : String -> List (Html msg) -> Html msg
liClass =
    elClass li


spanClass : String -> List (Html msg) -> Html msg
spanClass =
    elClass span


tableClass : String -> List (Html msg) -> Html msg
tableClass =
    elClass table


trClass : String -> List (Html msg) -> Html msg
trClass =
    elClass tr


tdClass : String -> List (Html msg) -> Html msg
tdClass =
    elClass td


thClass : String -> List (Html msg) -> Html msg
thClass =
    elClass th


tbodyClass : String -> List (Html msg) -> Html msg
tbodyClass =
    elClass tbody


theadClass : String -> List (Html msg) -> Html msg
theadClass =
    elClass thead


h1Class : String -> List (Html msg) -> Html msg
h1Class =
    elClass h1


h2Class : String -> List (Html msg) -> Html msg
h2Class =
    elClass h2


h3Class : String -> List (Html msg) -> Html msg
h3Class =
    elClass h3


aClassHrefExternal : String -> String -> List (Html msg) -> Html msg
aClassHrefExternal cls hrf =
    a [ class cls, href hrf, target "_blank" ]


aClassHrefInternal : String -> Route -> List (Html msg) -> Html msg
aClassHrefInternal cls hrf =
    a [ class cls, Route.href hrf, target "_blank" ]


buttonClass : String -> msg -> List (Html msg) -> Html msg
buttonClass cls msg =
    button [ onClick msg, class cls ]
