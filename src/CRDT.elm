module CRDT exposing
    ( CRDT
    , ResolvedCRDT
    , conflictDemo
    , demo
    , editors
    , empty
    , emptyResolved
    , isResolved
    , length
    , previewResolutionFor
    , resolve
    , resolveWithVersionOf
    , toString
    , update
    )

import Array
import CRDTPath exposing (CRDTPath)
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
    ( helloWorld, "HELLO WORLD" )


conflictDemo : ( CRDT, String )
conflictDemo =
    ( conflict, "" )


conflict : CRDT
conflict =
    { operations =
        [ { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 4 ], char = 'E', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 7 ], char = 'L', isTomb = False }
        , { userId = UserId.fromString "alice", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
        , { userId = UserId.fromString "alice", path = CRDTPath.demoPath [ 4 ], char = 'A', isTomb = False }
        , { userId = UserId.fromString "alice", path = CRDTPath.demoPath [ 7 ], char = 'L', isTomb = False }
        ]
    , seed = Random.initialSeed 42
    }


helloWorld : CRDT
helloWorld =
    { operations =
        [ { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 2 ], char = 'E', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 6 ], char = ' ', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 3 ], char = 'L', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 10 ], char = 'R', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 4 ], char = 'L', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 8 ], char = 'O', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 5 ], char = 'O', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 7 ], char = 'W', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 11 ], char = 'L', isTomb = False }
        , { userId = UserId.fromString "bob", path = CRDTPath.demoPath [ 13 ], char = 'D', isTomb = False }
        ]
    , seed = Random.initialSeed 42
    }


previewResolutionFor : UserId -> CRDT -> Result String ResolvedCRDT
previewResolutionFor userId crdt =
    let
        updatedOperations =
            List.filter (\operation -> operation.userId == userId) crdt.operations
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
