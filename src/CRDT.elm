module CRDT exposing (CRDT, Operation(..), demo, toString, update)

import Array


type alias CRDT =
    List Operation


type Operation
    = Insert UserId Path Char


type alias UserId =
    String


type alias Path =
    List Int



--Can't be a set because Insert is not comparable.


demo : CRDT
demo =
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


toString : CRDT -> String
toString crdt =
    crdt
        |> List.sortBy pathFromOperation
        |> List.map displayInsert
        |> String.concat


toCharsWithPath crdt =
    crdt
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
    Insert "bob" (pathBefore path crdt) char :: crdt


addCharAtEnd : Char -> CRDT -> CRDT
addCharAtEnd char crdt =
    Insert "bob" (pathAfter crdt) char :: crdt


crdtUntil supremumPath crdt =
    List.filter
        (\operation ->
            case operation of
                Insert _ path _ ->
                    path < supremumPath
        )
        crdt


pathAfter crdt =
    incrementPath (pathAtTheEndOf crdt)


pathBefore : Path -> CRDT -> Path
pathBefore path crdt =
    let
        supremumPath =
            path

        minPath =
            pathAtTheEndOf (crdtUntil supremumPath crdt)
    in
    if isSpaceBetween minPath supremumPath then
        choosePathBetween minPath supremumPath

    else
        newSubregister minPath


isSpaceBetween minPath supremumPath =
    case minPath of
        [ minNumber ] ->
            case supremumPath of
                [ supremumNumber ] ->
                    minNumber + 1 < supremumNumber

                _ ->
                    False

        _ ->
            False


choosePathBetween minPath supremumPath =
    List.map ((+) 1) minPath


newSubregister minPath =
    case minPath of
        [ minNumber ] ->
            [ minNumber, 0 ]

        _ ->
            minPath


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
            crdt
                |> List.sortBy pathFromOperation
                |> List.reverse
                |> List.head
                |> Maybe.map pathFromOperation
                |> Maybe.withDefault [ 0 ]
    in
    if maxPath == [ 15 ] then
        [ 15, 0 ]

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


seed =
    Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 8, 10, 11, 13 ]



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
