module Main exposing (main)

import Browser
import CRDT exposing (CRDT, ResolvedCRDT)
import CRDTPath
import Html exposing (Html, br, button, div, h2, input, p, strong, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


type Msg
    = UpdateCRDT String String
    | ToggleCRDTRendering


type alias Model =
    { crdt : CRDT, control : String, renderCRDT : Bool }


type alias UserId =
    String


init : Model
init =
    { crdt = CRDT.demo, control = CRDT.demoAsString, renderCRDT = True }


main : Program () Model Msg
main =
    Browser.sandbox { view = view, init = init, update = update }


view : Model -> Html Msg
view model =
    case CRDT.resolve model.crdt of
        Err error ->
            text error

        Ok resolvedCRDT ->
            div []
                [ control model resolvedCRDT
                , crdtInput "bob" resolvedCRDT
                , crdtInput "alice" resolvedCRDT
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


crdtInput : UserId -> ResolvedCRDT -> Html Msg
crdtInput userId resolvedCRDT =
    div []
        [ h2 [] [ text (userId ++ "'s CRDT") ]
        , input [ onInput (UpdateCRDT userId), value (CRDT.toString resolvedCRDT) ] []
        , br [] []
        ]


control : Model -> ResolvedCRDT -> Html Msg
control model resolvedCRDT =
    div []
        [ h2 [] [ text "control" ]
        , input [ onInput (UpdateCRDT "control"), value model.control ] []
        , div [] [ text model.control ]
        , if CRDT.toString resolvedCRDT == model.control then
            strong []
                [ text "Result matches! (Length: "
                , text (String.fromInt <| CRDT.length resolvedCRDT)
                , text ")"
                ]

          else
            strong []
                [ text "Out of Sync! (Length: "
                , text (String.fromInt <| CRDT.length resolvedCRDT)
                , text ")"
                ]
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
