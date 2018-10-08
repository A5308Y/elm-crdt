module CRDT exposing (Operation(..), demo, toString, update, zip)

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
    [ Insert "bob" [ 0 ] 'H'
    , Insert "bob" [ 1 ] 'E'
    , Insert "bob" [ 5 ] ' '
    , Insert "bob" [ 2 ] 'L'
    , Insert "bob" [ 10 ] 'R'
    , Insert "bob" [ 3 ] 'L'
    , Insert "bob" [ 8 ] 'O'
    , Insert "bob" [ 4 ] 'O'
    , Insert "bob" [ 6 ] 'W'
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
    let
        compareList =
            if String.length updatedString >= List.length (toCharsWithPath crdt) then
                zip (String.toList updatedString) (toCharsWithPath crdt)

            else
                reverseZip (toCharsWithPath crdt) (String.toList updatedString)
    in
    List.foldr updateOnce crdt compareList



--I can only add things to the list!


updateOnce : ( Maybe Char, Maybe ( Char, Path ) ) -> CRDT -> CRDT
updateOnce listEntry crdt =
    case listEntry of
        ( Just updatedChar, Just ( presentChar, path ) ) ->
            if updatedChar == presentChar then
                crdt

            else
                crdt

        ( Nothing, Just ( presentChar, path ) ) ->
            --Remove?
            crdt

        ( Just updatedChar, Nothing ) ->
            Insert "bob" (maximumPath crdt) updatedChar :: crdt

        ( Nothing, Nothing ) ->
            crdt


maximumPath : CRDT -> Path
maximumPath crdt =
    crdt
        |> List.sortBy pathFromOperation
        |> List.reverse
        |> List.head
        |> Maybe.map pathFromOperation
        |> Maybe.map (List.map ((+) 1))
        |> Maybe.withDefault [ crdtRegisterMaximum ]


crdtRegisterMaximum : Int
crdtRegisterMaximum =
    15


zip : List a -> List b -> List ( Maybe a, Maybe b )
zip firstList secondList =
    let
        firstArray =
            Array.fromList firstList

        secondArray =
            Array.fromList secondList
    in
    Array.indexedMap
        (\indexInFirst elementFromFirst -> ( Just elementFromFirst, Array.get indexInFirst secondArray ))
        firstArray
        |> Array.toList


reverseZip : List b -> List a -> List ( Maybe a, Maybe b )
reverseZip secondList firstList =
    let
        firstArray =
            Array.fromList firstList

        secondArray =
            Array.fromList secondList
    in
    Array.indexedMap
        (\indexInSecond elementFromSecond -> ( Array.get indexInSecond firstArray, Just elementFromSecond ))
        secondArray
        |> Array.toList



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
