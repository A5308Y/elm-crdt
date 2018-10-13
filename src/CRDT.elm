module CRDT exposing (CRDT, Operation(..), demo, toString, update)

import Array
import Random


type alias CRDT =
    { operations : List Operation, seed : Random.Seed }


type Operation
    = Insert UserId Path Char


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
        [ Insert "bob" [ 1 ] 'A'
        , Insert "bob" [ 2 ] 'B'
        , Insert "bob" [ 6 ] 'C'
        ]
    , seed = Random.initialSeed 42
    }


helloWorld =
    { operations =
        [ Insert "bob" [ 1 ] 'H'
        , Insert "bob" [ 2 ] 'E'
        , Insert "bob" [ 6 ] ' '
        , Insert "bob" [ 3 ] 'L'
        , Insert "bob" [ 10 ] 'R'
        , Insert "bob" [ 4 ] 'L'
        , Insert "bob" [ 8 ] 'O'
        , Insert "bob" [ 5 ] 'O'
        , Insert "bob" [ 7 ] 'W'
        , Insert "bob" [ 11 ] 'L'
        , Insert "bob" [ 13 ] 'D'
        ]
    , seed = Random.initialSeed 42
    }


toString : CRDT -> String
toString crdt =
    crdt.operations
        |> List.sortBy pathFromOperation
        |> List.map displayInsert
        |> String.concat


toCharsWithPath : CRDT -> List ( Char, Path )
toCharsWithPath crdt =
    crdt.operations
        |> List.sortBy pathFromOperation
        |> List.map charWithPath


charWithPath : Operation -> ( Char, Path )
charWithPath operation =
    case operation of
        Insert _ path char ->
            ( char, path )


displayInsert : Operation -> String
displayInsert operation =
    case operation of
        Insert _ _ char ->
            String.fromChar char


pathFromOperation : Operation -> Path
pathFromOperation operation =
    case operation of
        Insert _ path _ ->
            path


update : UserId -> String -> CRDT -> CRDT
update userId updatedString crdt =
    if String.length updatedString > List.length (toCharsWithPath crdt) then
        -- This could also be replace though...
        addCharsToCRDT updatedString (String.toList updatedString) (toCharsWithPath crdt) crdt

    else
        crdt


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


addCharBefore : Path -> Char -> CRDT -> CRDT
addCharBefore path char crdt =
    let
        ( newPath, newSeed ) =
            pathBefore path crdt
    in
    { crdt | operations = Insert "bob" newPath char :: crdt.operations, seed = newSeed }


addCharAtEnd : Char -> CRDT -> CRDT
addCharAtEnd char crdt =
    let
        ( newPath, newSeed ) =
            pathAfter crdt
    in
    { crdt | operations = Insert "bob" newPath char :: crdt.operations, seed = newSeed }


crdtUntil : Path -> CRDT -> CRDT
crdtUntil supremumPath crdt =
    let
        filteredOperations =
            List.filter
                (\operation ->
                    case operation of
                        Insert _ path _ ->
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
    let
        maxPath =
            crdt.operations
                |> List.sortBy pathFromOperation
                |> List.reverse
                |> List.head
                |> Maybe.map pathFromOperation
                |> Maybe.withDefault [ 0 ]
    in
    maxPath


crdtRegisterMaximum : Int
crdtRegisterMaximum =
    15


crdtRegisterMinimum : Int
crdtRegisterMinimum =
    0



--if String.length updatedString >  then
--    List.foldl
--List.foldl (String.toList updatedString) (CRDT.toCharsWithPath crdt)
--Fold starting with 0 setting the resulting path to the last compared, stopping when different
-- [('H', ('H', [1])), 'E', ('E', [3])]
--insert a character one after the other and be certain that the same seed is used so that the same operations are created again.
--Does not work. Scenario: alice and bob share "Hello". Alice edits "Hello" to say "Hi". Shares the result. Bob updates and everythin is removed again.
--I can't use CRDT.toString crdt because it might not be a valid string because multiple users might have added conflicting inserts
-- It might actually be alright as long as the CRDT version has different characters
--if String.length updatedString > String.length () then
--    String.startsWith updatedString CRDT.toString crdt
--1. For each character in the updatedString compare if the result of the crdt starts with the updatedString up to this character
--1.1. If True go to the next character
--1.2. If False save the position of the last Insert of the crdt that matched the updatedString as the start for alloc
--2. reverse the string and repeat one but use it to determine the end of the alloc instead of the start
