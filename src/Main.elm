module Main exposing (main)

import Browser
import CRDT exposing (CRDT)
import CRDTPath
import Html exposing (Html, br, button, div, h2, input, p, strong, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


type Msg
    = UpdateCRDT String String
    | ToggleCRDTRendering


type alias Model =
    { crdt : CRDT, control : String, renderCRDT : Bool }


init : Model
init =
    { crdt = CRDT.demo, control = CRDT.toString CRDT.demo, renderCRDT = True }


main : Program () Model Msg
main =
    Browser.sandbox { view = view, init = init, update = update }


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "control" ]
        , input [ onInput (UpdateCRDT "control"), value model.control ] []
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
        , h2 [] [ text "Bob's CRDT" ]
        , input [ onInput (UpdateCRDT "bob"), value (CRDT.toString model.crdt) ] []
        , br [] []
        , h2 [] [ text "Alice's CRDT" ]
        , input [ onInput (UpdateCRDT "alice"), value (CRDT.toString model.crdt) ] []
        , br [] []
        , button [ onClick ToggleCRDTRendering ] [ text "Disable CRDT rendering (for performance testing)" ]
        , if model.renderCRDT then
            div []
                (List.map
                    (\operation -> div [] [ text (Debug.toString operation) ])
                    (List.sortBy (.path >> CRDTPath.sortOrder) model.crdt.operations)
                )

          else
            text ""
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCRDT userId updatedString ->
            { model
                | crdt = CRDT.update userId updatedString model.crdt
                , control = updatedString
            }

        ToggleCRDTRendering ->
            { model | renderCRDT = False }
