module Html.Common exposing (..)

import Html exposing (Attribute, Html, a, b, button, div, h1, h2, h3, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Time exposing (Month(..), Posix, Zone)


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
    a [ class cls, Route.href hrf ]


buttonClass : String -> msg -> List (Html msg) -> Html msg
buttonClass cls msg =
    button [ onClick msg, class cls ]


dateView : Zone -> Posix -> Html msg
dateView zone time =
    let
        toMonth month_ =
            case month_ of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        year =
            String.fromInt (Time.toYear zone time)

        month =
            toMonth (Time.toMonth zone time)

        day =
            String.padLeft 2 '0' <|
                String.fromInt (Time.toDay zone time)
    in
    text <| String.join "-" [ year, month, day ]
