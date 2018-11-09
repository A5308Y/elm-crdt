module CRDT exposing
    ( CRDT
    , ResolvedCRDT
    , conflictDemo
    , decoder
    , demo
    , editors
    , empty
    , emptyResolved
    , encoder
    , equal
    , isResolved
    , length
    , merge
    , previewResolutionFor
    , resolve
    , resolveWithVersionOf
    , toString
    , update
    )

import Array
import CRDTPath exposing (CRDTPath)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Random
import Set
import UserId exposing (UserId)


type ResolvedCRDT
    = ResolvedCRDT CRDT


type alias CRDT =
    { operations : List Operation, seed : Random.Seed }


type alias Operation =
    { userId : UserId, path : CRDTPath, char : Char, isTomb : Bool }


empty : CRDT
empty =
    CRDT [] (Random.initialSeed 0)


emptyResolved : ResolvedCRDT
emptyResolved =
    ResolvedCRDT (CRDT [] (Random.initialSeed 0))


demo : ( CRDT, String )
demo =
    ( empty, "" )


conflictDemo : ( CRDT, String )
conflictDemo =
    ( conflict, "" )


conflict : CRDT
conflict =
    { operations =
        [ Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 1 ]) 'H' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 4 ]) 'E' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 7 ]) 'L' False
        , Operation (UserId.fromString "alice") (CRDTPath.demoPath [ 1 ]) 'H' False
        , Operation (UserId.fromString "alice") (CRDTPath.demoPath [ 4 ]) 'A' False
        , Operation (UserId.fromString "alice") (CRDTPath.demoPath [ 7 ]) 'L' False
        ]
    , seed = Random.initialSeed 42
    }


helloWorld : CRDT
helloWorld =
    { operations =
        [ Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 1 ]) 'H' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 2 ]) 'E' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 6 ]) ' ' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 3 ]) 'L' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 10 ]) 'R' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 4 ]) 'L' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 8 ]) 'O' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 5 ]) 'O' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 7 ]) 'W' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 11 ]) 'L' False
        , Operation (UserId.fromString "bob") (CRDTPath.demoPath [ 13 ]) 'D' False
        ]
    , seed = Random.initialSeed 42
    }


previewResolutionFor : UserId -> CRDT -> Result String ResolvedCRDT
previewResolutionFor userId crdt =
    let
        tombs =
            List.filter
                .isTomb
                crdt.operations

        thisUsersNonTombOperations =
            List.filter
                (\operation -> not operation.isTomb && operation.userId == userId)
                crdt.operations

        nonTombOperationsFromOtherUsersWithNewPaths =
            List.filter
                (\operation -> not (List.member operation.path (List.map .path thisUsersNonTombOperations)))
                crdt.operations

        updatedOperations =
            tombs ++ thisUsersNonTombOperations ++ nonTombOperationsFromOtherUsersWithNewPaths
    in
    resolve { crdt | operations = updatedOperations }


resolveWithVersionOf : UserId -> CRDT -> CRDT
resolveWithVersionOf userId crdt =
    let
        paths =
            List.map .path crdt.operations

        duplicatePaths =
            List.filter
                (\outerPath -> List.length (List.filter (\innerPath -> innerPath == outerPath) paths) > 1)
                paths
    in
    { crdt | operations = List.map (resolveOperation userId duplicatePaths) crdt.operations }


resolveOperation : UserId -> List CRDTPath -> Operation -> Operation
resolveOperation userId duplicatePaths operation =
    if List.member operation.path duplicatePaths && operation.userId /= userId && not operation.isTomb then
        { operation | isTomb = True }

    else
        operation


resolve : CRDT -> Result String ResolvedCRDT
resolve crdt =
    if isResolved crdt then
        Ok (ResolvedCRDT crdt)

    else
        Err "Can't resolve crdt"


toString : ResolvedCRDT -> String
toString (ResolvedCRDT crdt) =
    crdt.operations
        |> List.filter (not << .isTomb)
        |> List.sortBy (.path >> CRDTPath.sortOrder)
        |> List.map .char
        |> String.fromList


toCharsWithPath : CRDT -> List ( Char, CRDTPath )
toCharsWithPath crdt =
    crdt.operations
        |> List.filter (not << .isTomb)
        |> List.sortBy (.path >> CRDTPath.sortOrder)
        |> List.map (\operation -> ( operation.char, operation.path ))


update : UserId -> String -> CRDT -> CRDT
update userId updatedString crdt =
    let
        ( infimumPath, charsAfterInfimum, unmatchedCharsWithPaths ) =
            findLastMatchingPath
                (String.toList updatedString)
                (toCharsWithPath crdt)
                CRDTPath.absoluteInfimum

        ( supremumPath, charsBetween, stillUnmatchedCharsWithPaths ) =
            findLastMatchingPath
                (List.reverse charsAfterInfimum)
                (List.reverse unmatchedCharsWithPaths)
                CRDTPath.absoluteSupremum
    in
    crdt
        |> markBetweenAsTomb infimumPath supremumPath
        |> insertCharsBetween userId infimumPath supremumPath (List.reverse charsBetween)


findLastMatchingPath : List Char -> List ( Char, CRDTPath ) -> CRDTPath -> ( CRDTPath, List Char, List ( Char, CRDTPath ) )
findLastMatchingPath chars charsWithPaths currentPath =
    case charsWithPaths of
        ( charFromCRDT, path ) :: restCharsWithPath ->
            case chars of
                charFromString :: restCharsFromString ->
                    if charFromCRDT == charFromString then
                        findLastMatchingPath restCharsFromString restCharsWithPath path

                    else
                        ( currentPath, chars, charsWithPaths )

                [] ->
                    ( currentPath, chars, restCharsWithPath )

        [] ->
            ( currentPath, chars, [] )


markBetweenAsTomb : CRDTPath -> CRDTPath -> CRDT -> CRDT
markBetweenAsTomb infimumPath supremumPath givenCrdt =
    { givenCrdt
        | operations = List.map (markOperationAsTomb infimumPath supremumPath) givenCrdt.operations
    }


markOperationAsTomb : CRDTPath -> CRDTPath -> Operation -> Operation
markOperationAsTomb infimumPath supremumPath operation =
    if CRDTPath.isBetween infimumPath supremumPath operation.path then
        { operation | isTomb = True }

    else
        operation


insertCharsBetween : UserId -> CRDTPath -> CRDTPath -> List Char -> CRDT -> CRDT
insertCharsBetween userId infimumPath supremumPath chars crdt =
    let
        tombAdjustedInfimum =
            crdt.operations
                |> List.filter .isTomb
                |> List.map .path
                |> CRDTPath.findPathExcluding infimumPath supremumPath

        ( chosenPath, newSeed ) =
            CRDTPath.choosePathBetween crdt.seed tombAdjustedInfimum supremumPath
    in
    case chars of
        char :: restChars ->
            let
                newOperation =
                    Operation userId chosenPath char False

                updatedCRDT =
                    { crdt | operations = newOperation :: crdt.operations, seed = newSeed }
            in
            insertCharsBetween userId chosenPath supremumPath restChars updatedCRDT

        [] ->
            crdt


isResolved : CRDT -> Bool
isResolved crdt =
    crdt.operations
        |> List.filter (not << .isTomb)
        |> List.map .path
        |> CRDTPath.allDifferent


length : ResolvedCRDT -> Int
length (ResolvedCRDT crdt) =
    List.length crdt.operations


editors : CRDT -> List UserId
editors crdt =
    crdt.operations
        |> List.map (\operation -> UserId.toString operation.userId)
        |> Set.fromList
        |> Set.toList
        |> List.map (\string -> UserId.fromString string)


encoder : CRDT -> Json.Encode.Value
encoder crdt =
    let
        ( randomInt, nextSeed ) =
            Random.step (Random.int 0 100) crdt.seed
    in
    Json.Encode.object
        [ ( "operations", Json.Encode.list encodeOperation crdt.operations )
        , ( "seed", Json.Encode.int randomInt )
        ]


encodeOperation : Operation -> Json.Encode.Value
encodeOperation operation =
    Json.Encode.object
        [ ( "userId", Json.Encode.string <| UserId.toString operation.userId )
        , ( "path", CRDTPath.encode operation.path )
        , ( "char", Json.Encode.string <| String.fromChar operation.char )
        , ( "isTomb", Json.Encode.bool operation.isTomb )
        ]


operationDecoder : Json.Decode.Decoder Operation
operationDecoder =
    Json.Decode.succeed operationConstructor
        |> Json.Decode.Pipeline.required "userId" Json.Decode.string
        |> Json.Decode.Pipeline.required "path" (Json.Decode.list Json.Decode.int)
        |> Json.Decode.Pipeline.required "char" Json.Decode.string
        |> Json.Decode.Pipeline.required "isTomb" Json.Decode.bool


operationConstructor : String -> List Int -> String -> Bool -> Operation
operationConstructor userIdString intList stringChar isTomb =
    Operation
        (UserId.fromString userIdString)
        (CRDTPath.demoPath intList)
        (Maybe.withDefault ' ' <| List.head <| String.toList stringChar)
        isTomb


decoder : Json.Decode.Decoder CRDT
decoder =
    Json.Decode.succeed crdtConstructor
        |> Json.Decode.Pipeline.required "operations" (Json.Decode.list operationDecoder)
        |> Json.Decode.Pipeline.required "seed" Json.Decode.int


crdtConstructor : List Operation -> Int -> CRDT
crdtConstructor operations seedInt =
    CRDT operations (Random.initialSeed seedInt)


merge : CRDT -> CRDT -> CRDT
merge leftCrdt rightCrdt =
    let
        allOperations =
            leftCrdt.operations ++ rightCrdt.operations

        updatedOperations =
            List.foldl uniqueFold [] allOperations
    in
    { seed = leftCrdt.seed, operations = updatedOperations }


uniqueFold : Operation -> List Operation -> List Operation
uniqueFold operation acc =
    if List.member (operationToComparable operation) (List.map operationToComparable acc) then
        acc

    else
        operation :: acc


operationToComparable : Operation -> ( CRDTPath, UserId )
operationToComparable operation =
    ( operation.path, operation.userId )


equal : CRDT -> CRDT -> Bool
equal leftCrdt rightCrdt =
    List.sortBy operationOrder leftCrdt.operations == List.sortBy operationOrder rightCrdt.operations


operationOrder : Operation -> ( List Int, String )
operationOrder operation =
    ( CRDTPath.sortOrder operation.path, UserId.toString operation.userId )
