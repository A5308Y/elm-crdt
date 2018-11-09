module Main exposing (main)

import Browser
import CRDT exposing (CRDT, ResolvedCRDT)
import CRDTPath
import Html exposing (Html, a, br, button, div, h2, input, li, p, strong, text, ul)
import Html.Attributes exposing (href, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import UserId exposing (UserId)


type Msg
    = UpdateCRDT UserId String
    | ToggleCRDTRendering
    | ChooseCRDTVersion UserId CRDT
    | ReceiveBinConfirmation (Result Http.Error PasteBinResult)
    | TriggerSynchronize
    | Synchronize (Result Http.Error CRDT)
    | ReceiveFromBin (Result Http.Error CRDT)
    | CreateNew


type alias PasteBinResult =
    { uri : String }


type alias Model =
    { crdt : CRDT, control : String, renderCRDT : Bool }


init : flags -> ( Model, Cmd Msg )
init flags =
    let
        ( crdt, initialControl ) =
            CRDT.demo
    in
    ( Model crdt initialControl True, Http.send Synchronize getFromBin )


main : Program () Model Msg
main =
    Browser.element { view = view, init = init, update = update, subscriptions = \model -> Sub.none }


view : Model -> Html Msg
view model =
    case CRDT.resolve model.crdt of
        Err error ->
            div []
                [ text "There are conflicting edits. The following users have created the following versions: "
                , ul [] (List.map (usersVersion model.crdt) (CRDT.editors model.crdt) ++ [ dontDecideOption ])
                ]

        Ok resolvedCRDT ->
            div []
                [ control model resolvedCRDT
                , crdtInput (UserId.fromString "bob") resolvedCRDT
                , crdtInput (UserId.fromString "alice") resolvedCRDT
                , debugOutput model
                , button [ onClick CreateNew ] [ text "Create New Empty CRDT" ]
                , button [ onClick TriggerSynchronize ] [ text "Synchronize" ]
                ]


dontDecideOption =
    li [] [ text "Don't decide and keep working on my version" ]


usersVersion : CRDT -> UserId -> Html Msg
usersVersion crdt userId =
    case CRDT.previewResolutionFor userId crdt of
        Ok resolvedCRDT ->
            li []
                [ text (UserId.toString userId ++ " (" ++ CRDT.toString resolvedCRDT ++ ")")
                , a [ href "#", onClick (ChooseCRDTVersion userId crdt) ] [ text "Choose this Version" ]
                ]

        Err message ->
            li [] [ text (Debug.toString crdt) ]


debugOutput : Model -> Html Msg
debugOutput model =
    div []
        [ button [ onClick ToggleCRDTRendering ] [ text "Disable CRDT rendering (for performance testing)" ]
        , if model.renderCRDT then
            div []
                ([ text (Debug.toString model.crdt.seed) ]
                    ++ List.map
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCRDT userId updatedString ->
            ( { model
                | crdt = CRDT.update userId updatedString model.crdt
                , control = updatedString
              }
            , Cmd.none
            )

        ToggleCRDTRendering ->
            ( { model | renderCRDT = False }, Cmd.none )

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
            ( { model | crdt = resolvableCRDT, control = updatedControl }
            , Http.send ReceiveFromBin (putToBin (CRDT.encoder resolvableCRDT))
            )

        TriggerSynchronize ->
            ( model, Http.send Synchronize getFromBin )

        CreateNew ->
            ( { model | crdt = CRDT.empty, control = "" }
            , Http.send ReceiveBinConfirmation (postToBin (CRDT.encoder CRDT.empty))
            )

        ReceiveBinConfirmation result ->
            let
                debug =
                    Debug.log "result" result
            in
            ( model, Cmd.none )

        Synchronize result ->
            case result of
                Err message ->
                    ( { model | control = Debug.toString message }, Cmd.none )

                Ok foreignCrdt ->
                    let
                        updatedCRDT =
                            CRDT.merge foreignCrdt model.crdt

                        updatedControl =
                            case CRDT.resolve updatedCRDT of
                                Err message ->
                                    message

                                Ok resolvedCRDT ->
                                    CRDT.toString resolvedCRDT
                    in
                    ( { model | crdt = updatedCRDT, control = updatedControl }
                    , if CRDT.equal updatedCRDT foreignCrdt then
                        Cmd.none

                      else
                        Http.send ReceiveFromBin (putToBin (CRDT.encoder updatedCRDT))
                    )

        ReceiveFromBin result ->
            case result of
                Err message ->
                    ( { model | control = Debug.toString message }, Cmd.none )

                Ok foreignCrdt ->
                    let
                        updatedCRDT =
                            CRDT.merge foreignCrdt model.crdt

                        updatedControl =
                            case CRDT.resolve updatedCRDT of
                                Err message ->
                                    message

                                Ok resolvedCRDT ->
                                    CRDT.toString resolvedCRDT
                    in
                    ( { model | crdt = updatedCRDT, control = updatedControl }
                    , Cmd.none
                    )


getFromBin : Http.Request CRDT
getFromBin =
    Http.get ("https://api.myjson.com/bins/" ++ binId) CRDT.decoder


binId : String
binId =
    "c57b2"


baseBinUri : String
baseBinUri =
    "https://api.myjson.com/bins"


postToBin : Json.Encode.Value -> Http.Request PasteBinResult
postToBin jsonValue =
    Http.post baseBinUri (Http.jsonBody jsonValue) pasteBinResultDecoder


putToBin : Json.Encode.Value -> Http.Request CRDT
putToBin jsonValue =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "https://api.myjson.com/bins/" ++ binId
        , body = Http.jsonBody jsonValue
        , expect = Http.expectJson CRDT.decoder
        , timeout = Nothing
        , withCredentials = False
        }


pasteBinResultDecoder : Json.Decode.Decoder PasteBinResult
pasteBinResultDecoder =
    Json.Decode.succeed PasteBinResult
        |> Json.Decode.Pipeline.required "uri" Json.Decode.string
