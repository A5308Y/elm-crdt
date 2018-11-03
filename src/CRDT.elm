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
    let
        charsWithPaths =
            toCharsWithPath crdt

        charList =
            String.toList updatedString

        ( infimumPath, charsAfterInfimum, unmatchedCharsWithPaths ) =
            findLastMatchingPath charList charsWithPaths [ 0 ]

        ( supremumPath, charsBetween, stillUnmatchedCharsWithPaths ) =
            findLastMatchingPath (List.reverse charsAfterInfimum) (List.reverse unmatchedCharsWithPaths) [ 15 ]
    in
    crdt
        |> markBetweenAsTomb infimumPath supremumPath
        |> insertCharsBetween infimumPath supremumPath (List.reverse charsBetween)


findLastMatchingPath : List Char -> List ( Char, Path ) -> Path -> ( Path, List Char, List ( Char, Path ) )
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


markBetweenAsTomb : Path -> Path -> CRDT -> CRDT
markBetweenAsTomb infimumPath supremumPath givenCrdt =
    { givenCrdt
        | operations = List.map (markOperationAsTomb infimumPath supremumPath) givenCrdt.operations
    }


markOperationAsTomb infimumPath supremumPath operation =
    if operation.path > infimumPath && operation.path < supremumPath then
        { operation | isTomb = True }

    else
        operation


insertCharsBetween : Path -> Path -> List Char -> CRDT -> CRDT
insertCharsBetween infimumPath supremumPath chars crdt =
    let
        ( chosenPath, newSeed ) =
            choosePathBetween crdt.seed infimumPath supremumPath
    in
    case chars of
        char :: restChars ->
            let
                newOperation =
                    { userId = "bob", path = chosenPath, char = char, isTomb = False }

                updatedCRDT =
                    { crdt | operations = newOperation :: crdt.operations }
            in
            insertCharsBetween chosenPath supremumPath restChars updatedCRDT

        [] ->
            crdt


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
