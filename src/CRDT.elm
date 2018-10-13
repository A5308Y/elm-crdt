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
    ( incrementPath (pathAtTheEndOf crdt), crdt.seed )


pathBefore : Path -> CRDT -> ( Path, Random.Seed )
pathBefore path crdt =
    let
        supremumPath =
            path

        minPath =
            pathAtTheEndOf (crdtUntil supremumPath crdt)
    in
    choosePathBetween crdt.seed minPath supremumPath


choosePathBetween : Random.Seed -> Path -> Path -> ( Path, Random.Seed )
choosePathBetween seed minPath supremumPath =
    case minPath of
        minNumber :: restMinPath ->
            case supremumPath of
                supremumNumber :: restSupremumPath ->
                    if minNumber + 1 == supremumNumber then
                        if List.isEmpty restMinPath then
                            newSubregister seed minPath

                        else
                            choosePathBetween seed restMinPath restSupremumPath

                    else
                        let
                            ( number, nextSeed ) =
                                Random.step (Random.int (minNumber + 1) supremumNumber) seed
                        in
                        ( List.map ((+) number) minPath, nextSeed )

                [] ->
                    Debug.todo "Implement Choice for nested paths (inner)"

        [] ->
            Debug.todo "Implement Choice for nested paths (outer)"


newSubregister : Random.Seed -> Path -> ( Path, Random.Seed )
newSubregister seed minPath =
    case minPath of
        [ minNumber ] ->
            let
                ( number, nextSeed ) =
                    Random.step (Random.int minNumber crdtRegisterMaximum) seed
            in
            ( [ minNumber, number ], nextSeed )

        _ ->
            ( minPath, seed )


incrementPath : Path -> Path
incrementPath path =
    case path of
        [ number ] ->
            [ number + 1 ]

        _ ->
            path


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
    if maxPath == [ crdtRegisterMaximum ] then
        [ crdtRegisterMaximum, 0 ]

    else
        maxPath


crdtRegisterMaximum : Int
crdtRegisterMaximum =
    15



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
