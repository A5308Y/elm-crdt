module Main exposing (main)

import CRDT
import Html


main : Html.Html msg
main =
    Html.text (CRDT.toString CRDT.demo)
