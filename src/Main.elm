module Main exposing (main)

import Browser
import CRDT exposing (CRDT, ResolvedCRDT)
import CRDTPath
import Html exposing (Html, a, br, button, div, h2, input, li, p, strong, text, ul)
import Html.Attributes exposing (href, value)
import Html.Events exposing (onClick, onInput)
import UserId exposing (UserId)


type Msg
    = UpdateCRDT UserId String
    | ToggleCRDTRendering
    | ChooseCRDTVersion UserId CRDT


type alias Model =
    { crdt : CRDT, control : String, renderCRDT : Bool }


init : Model
init =
    let
        ( crdt, initialControl ) =
            CRDT.conflictDemo
    in
    Model crdt initialControl True


main : Program () Model Msg
main =
    Browser.sandbox { view = view, init = init, update = update }


view : Model -> Html Msg
view model =
    case CRDT.resolve model.crdt of
        Err error ->
            div []
                [ text "There are conflicting edits. The following users have created the following versions: "
                , ul [] (List.map (usersVersion model.crdt) (CRDT.editors model.crdt))
                ]

        Ok resolvedCRDT ->
            div []
                [ control model resolvedCRDT
                , crdtInput (UserId.fromString "bob") resolvedCRDT
                , crdtInput (UserId.fromString "alice") resolvedCRDT
                , debugOutput model
                ]


usersVersion : CRDT -> UserId -> Html Msg
usersVersion crdt userId =
    case CRDT.previewResolutionFor userId crdt of
        Ok resolvedCRDT ->
            li []
                [ text (UserId.toString userId ++ " (" ++ CRDT.toString resolvedCRDT ++ ")")
                , a [ href "#", onClick (ChooseCRDTVersion userId crdt) ] [ text "Choose this Version" ]
                ]

        Err message ->
            li [] [ text message ]


debugOutput : Model -> Html Msg
debugOutput model =
    div []
        [ button [ onClick ToggleCRDTRendering ] [ text "Disable CRDT rendering (for performance testing)" ]
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
        [ h2 [] [ text (UserId.toString userId ++ "'s CRDT") ]
        , input [ onInput (UpdateCRDT userId), value (CRDT.toString resolvedCRDT) ] []
        , br [] []
        ]


control : Model -> ResolvedCRDT -> Html Msg
control model resolvedCRDT =
    div []
        [ h2 [] [ text "control" ]
        , input [ onInput (UpdateCRDT (UserId.fromString "control")), value model.control ] []
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

        ChooseCRDTVersion userId crdt ->
            let
                resolvableCRDT =
                    CRDT.resolveWithVersionOf userId crdt

                updatedControl =
                    case CRDT.resolve resolvableCRDT of
                        Ok resolvedCRDT ->
                            CRDT.toString resolvedCRDT

                        Err message ->
                            message
            in
            { model | crdt = resolvableCRDT, control = updatedControl }
