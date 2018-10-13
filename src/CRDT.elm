module CRDT exposing (CRDT, Operation(..), demo, toString, update)

import Array
import Random


type alias CRDT =
    { operations : List Operation, seed : Random.Seed }


type Operation
    = Insert UserId Path Char Bool


type alias UserId =
    String


type alias Path =
    List Int



--Can't be a set because Insert is not comparable.


demo : CRDT
demo =
    abc


abc =
    { operations =
        [ Insert "bob" [ 1 ] 'A' False
        , Insert "bob" [ 2 ] 'B' False
        , Insert "bob" [ 6 ] 'C' False
        ]
    , seed = Random.initialSeed 42
    }


helloWorld =
    { operations =
        [ Insert "bob" [ 1 ] 'H' False
        , Insert "bob" [ 2 ] 'E' False
        , Insert "bob" [ 6 ] ' ' False
        , Insert "bob" [ 3 ] 'L' False
        , Insert "bob" [ 10 ] 'R' False
        , Insert "bob" [ 4 ] 'L' False
        , Insert "bob" [ 8 ] 'O' False
        , Insert "bob" [ 5 ] 'O' False
        , Insert "bob" [ 7 ] 'W' False
        , Insert "bob" [ 11 ] 'L' False
        , Insert "bob" [ 13 ] 'D' False
        ]
    , seed = Random.initialSeed 42
    }


toString : CRDT -> String
toString crdt =
    crdt.operations
        |> List.filter (removeTombs crdt.operations)
        |> List.sortBy pathFromOperation
        |> List.map displayInsert
        |> String.concat


removeTombs operations operation =
    case operation of
        Insert _ _ _ isTomb ->
            not isTomb


toCharsWithPath : CRDT -> List ( Char, Path )
toCharsWithPath crdt =
    crdt.operations
        |> List.filter (removeTombs crdt.operations)
        |> List.sortBy pathFromOperation
        |> List.map charWithPath


charWithPath : Operation -> ( Char, Path )
charWithPath operation =
    case operation of
        Insert _ path char _ ->
            ( char, path )


displayInsert : Operation -> String
displayInsert operation =
    case operation of
        Insert _ _ char _ ->
            String.fromChar char


pathFromOperation : Operation -> Path
pathFromOperation operation =
    case operation of
        Insert _ path _ _ ->
            path


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

        Just ( index, Insert userId path char _ ) ->
            let
                updatedOperations =
                    crdt.operations
                        |> Array.fromList
                        |> Array.set index (Insert userId path char True)
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
pathFilter queriedPath ( index, Insert _ path _ _ ) =
    queriedPath == path


addCharBefore : Path -> Char -> CRDT -> CRDT
addCharBefore path char crdt =
    let
        ( newPath, newSeed ) =
            pathBefore path crdt
    in
    { crdt | operations = Insert "bob" newPath char False :: crdt.operations, seed = newSeed }


addCharAtEnd : Char -> CRDT -> CRDT
addCharAtEnd char crdt =
    let
        ( newPath, newSeed ) =
            pathAfter crdt
    in
    { crdt | operations = Insert "bob" newPath char False :: crdt.operations, seed = newSeed }


crdtUntil : Path -> CRDT -> CRDT
crdtUntil supremumPath crdt =
    let
        filteredOperations =
            List.filter
                (\operation ->
                    case operation of
                        Insert _ path _ _ ->
                            path < supremumPath
                )
                crdt.operations
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


nextBetweenStep seed infimum supremum =
    let
        ( randomInt, nextSeed ) =
            Random.step (Random.int (infimum + 1) (supremum - 1)) seed
    in
    ( [ randomInt ], nextSeed )


pathAtTheEndOf : CRDT -> Path
pathAtTheEndOf crdt =
    crdt.operations
        |> List.sortBy pathFromOperation
        |> List.reverse
        |> List.head
        |> Maybe.map pathFromOperation
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
