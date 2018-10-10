module Main exposing (main)

import CRDT
import Html exposing (div, input)


main : Html.Html msg
main =
    div []
        [ Html.text (CRDT.toString CRDT.demo)
        , input [] []
        ]
