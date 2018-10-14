module CRDT exposing (CRDT, demo, toString, update)

import Array
import Random


type alias CRDT =
    { operations : List Operation, seed : Random.Seed }


type alias Operation =
    { userId : UserId, path : Path, char : Char, isTomb : Bool }


type alias UserId =
    String


type alias Path =
    List Int



--Can't be a set because Insert is not comparable.


demo : CRDT
demo =
    abc


abc : CRDT
abc =
    { operations =
        [ { userId = "bob", path = [ 1 ], char = 'A', isTomb = False }
        , { userId = "bob", path = [ 2 ], char = 'B', isTomb = False }
        , { userId = "bob", path = [ 6 ], char = 'C', isTomb = False }
        ]
    , seed = Random.initialSeed 42
    }


helloWorld : CRDT
helloWorld =
    { operations =
        [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
        , { userId = "bob", path = [ 2 ], char = 'E', isTomb = False }
        , { userId = "bob", path = [ 6 ], char = ' ', isTomb = False }
        , { userId = "bob", path = [ 3 ], char = 'L', isTomb = False }
        , { userId = "bob", path = [ 10 ], char = 'R', isTomb = False }
        , { userId = "bob", path = [ 4 ], char = 'L', isTomb = False }
        , { userId = "bob", path = [ 8 ], char = 'O', isTomb = False }
        , { userId = "bob", path = [ 5 ], char = 'O', isTomb = False }
        , { userId = "bob", path = [ 7 ], char = 'W', isTomb = False }
        , { userId = "bob", path = [ 11 ], char = 'L', isTomb = False }
        , { userId = "bob", path = [ 13 ], char = 'D', isTomb = False }
        ]
    , seed = Random.initialSeed 42
    }


toString : CRDT -> String
toString crdt =
    crdt.operations
        |> List.filter (not << .isTomb)
        |> List.sortBy .path
        |> List.map .char
        |> String.fromList


toCharsWithPath : CRDT -> List ( Char, Path )
toCharsWithPath crdt =
    crdt.operations
        |> List.filter (not << .isTomb)
        |> List.sortBy .path
        |> List.map (\operation -> ( operation.char, operation.path ))


update : UserId -> String -> CRDT -> CRDT
update userId updatedString crdt =
    if String.length updatedString > List.length (toCharsWithPath crdt) then
        addCharsToCRDT updatedString (String.toList updatedString) (toCharsWithPath crdt) crdt

    else
        removeCharsFromCRDT updatedString (String.toList updatedString) (toCharsWithPath crdt) crdt


addCharsToCRDT : String -> List Char -> List ( Char, Path ) -> CRDT -> CRDT
addCharsToCRDT initialString charsToAdd charPathList crdt =
    case charsToAdd of
        currentCharToAdd :: restCharsToAdd ->
            case charPathList of
                ( currentCharWithPath, currentPath ) :: restCharsWithPath ->
                    if currentCharToAdd == currentCharWithPath then
                        addCharsToCRDT initialString restCharsToAdd restCharsWithPath crdt

                    else
                        update "bob" initialString (addCharBefore currentPath currentCharToAdd crdt)

                [] ->
                    update "bob" initialString (addCharAtEnd currentCharToAdd crdt)

        [] ->
            crdt


removeCharsFromCRDT : String -> List Char -> List ( Char, Path ) -> CRDT -> CRDT
removeCharsFromCRDT initialString charsToKeep charPathList crdt =
    case charPathList of
        ( currentCharWithPath, currentPath ) :: restCharsWithPath ->
            case charsToKeep of
                currentCharToKeep :: restCharsToKeep ->
                    if currentCharToKeep == currentCharWithPath then
                        removeCharsFromCRDT initialString restCharsToKeep restCharsWithPath crdt

                    else
                        crdt
                            |> markAsTomb currentPath
                            |> update "bob" initialString

                [] ->
                    crdt
                        |> markAsTomb currentPath
                        |> update "bob" initialString

        [] ->
            crdt


markAsTomb : Path -> CRDT -> CRDT
markAsTomb targetPath crdt =
    case findOperation targetPath crdt of
        Nothing ->
            crdt

        Just ( index, operation ) ->
            let
                updatedOperations =
                    crdt.operations
                        |> Array.fromList
                        |> Array.set index { operation | isTomb = True }
                        |> Array.toList
            in
            { crdt | operations = updatedOperations, seed = crdt.seed }


findOperation : Path -> CRDT -> Maybe ( Int, Operation )
findOperation path crdt =
    crdt.operations
        |> Array.fromList
        |> Array.indexedMap (\index operation -> ( index, operation ))
        |> Array.filter (pathFilter path)
        |> Array.get 0


pathFilter : Path -> ( Int, Operation ) -> Bool
pathFilter queriedPath ( index, operation ) =
    queriedPath == operation.path


addCharBefore : Path -> Char -> CRDT -> CRDT
addCharBefore path char crdt =
    let
        ( newPath, newSeed ) =
            pathBefore path crdt
    in
    { crdt
        | operations = { userId = "bob", path = newPath, char = char, isTomb = False } :: crdt.operations
        , seed = newSeed
    }


addCharAtEnd : Char -> CRDT -> CRDT
addCharAtEnd char crdt =
    let
        ( newPath, newSeed ) =
            pathAfter crdt
    in
    { crdt
        | operations = { userId = "bob", path = newPath, char = char, isTomb = False } :: crdt.operations
        , seed = newSeed
    }


crdtUntil : Path -> CRDT -> CRDT
crdtUntil supremumPath crdt =
    let
        filteredOperations =
            List.filter (\operation -> operation.path < supremumPath) crdt.operations
    in
    { crdt | operations = filteredOperations, seed = crdt.seed }


pathAfter : CRDT -> ( Path, Random.Seed )
pathAfter crdt =
    let
        supremumPath =
            [ crdtRegisterMaximum ]

        infimumPath =
            pathAtTheEndOf (crdtUntil supremumPath crdt)
    in
    choosePathBetween crdt.seed infimumPath supremumPath


pathBefore : Path -> CRDT -> ( Path, Random.Seed )
pathBefore path crdt =
    let
        supremumPath =
            path

        infimumPath =
            pathAtTheEndOf (crdtUntil supremumPath crdt)
    in
    choosePathBetween crdt.seed infimumPath supremumPath


choosePathBetween : Random.Seed -> Path -> Path -> ( Path, Random.Seed )
choosePathBetween seed infimumPath supremumPath =
    case infimumPath of
        infimumHead :: infimumTail ->
            case supremumPath of
                supremumHead :: supremumTail ->
                    if infimumHead + 1 >= supremumHead then
                        let
                            ( path, nextSeed ) =
                                choosePathBetween seed infimumTail supremumTail
                        in
                        ( infimumHead :: path, nextSeed )

                    else
                        nextBetweenStep seed infimumHead supremumHead

                [] ->
                    choosePathBetween seed infimumPath [ crdtRegisterMaximum ]

        [] ->
            case supremumPath of
                supremumHead :: supremumTail ->
                    nextBetweenStep seed crdtRegisterMinimum supremumHead

                [] ->
                    nextBetweenStep seed crdtRegisterMinimum crdtRegisterMaximum


nextBetweenStep : Random.Seed -> Int -> Int -> ( Path, Random.Seed )
nextBetweenStep seed infimum supremum =
    let
        ( randomInt, nextSeed ) =
            Random.step (Random.int (infimum + 1) (supremum - 1)) seed
    in
    ( [ randomInt ], nextSeed )


pathAtTheEndOf : CRDT -> Path
pathAtTheEndOf crdt =
    crdt.operations
        |> List.sortBy .path
        |> List.reverse
        |> List.head
        |> Maybe.map .path
        |> Maybe.withDefault [ 0 ]


crdtRegisterMaximum : Int
crdtRegisterMaximum =
    15


crdtRegisterMinimum : Int
crdtRegisterMinimum =
    0



--insert a character one after the other and be certain that the same seed is used so that the same operations are created again.
--Does not work. Scenario: alice and bob share "Hello". Alice edits "Hello" to say "Hi". Shares the result. Bob updates and everything is removed again.
--I can't use CRDT.toString crdt because it might not be a valid string because multiple users might have added conflicting inserts
-- It might actually be alright as long as the CRDT version has different characters
