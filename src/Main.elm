module Main exposing (main)

import Browser
import CRDT exposing (CRDT)
import Html exposing (Html, br, div, h2, input, strong, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


type Msg
    = UpdateCRDT String


type alias Model =
    { crdt : CRDT, control : String }


init : Model
init =
    { crdt = CRDT.demo, control = CRDT.toString CRDT.demo }


main : Program () Model Msg
main =
    Browser.sandbox { view = view, init = init, update = update }


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "control" ]
        , input [ onInput UpdateCRDT, value model.control ] []
        , div [] [ text model.control ]
        , if CRDT.toString model.crdt == model.control then
            strong []
                [ text "Result matches! (Length: "
                , text (String.fromInt (List.length model.crdt.operations))
                , text ")"
                ]

          else
            strong []
                [ text "Out of Sync! (Length: "
                , text (String.fromInt (List.length model.crdt.operations))
                , text ")"
                ]
        , h2 [] [ text "CRDT" ]
        , input [ onInput UpdateCRDT, value (CRDT.toString model.crdt) ] []
        , br [] []
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCRDT updatedString ->
            let
                boundedString =
                    if String.length updatedString > 500 then
                        String.slice 0 500 updatedString

                    else
                        updatedString
            in
            { model
                | crdt = CRDT.update "bob" boundedString model.crdt
                , control = boundedString
            }
