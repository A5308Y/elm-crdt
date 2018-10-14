module Example exposing (suite)

import CRDT
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Test exposing (..)


suite : Test
suite =
    describe "CRDT"
        [ describe "CRDT.toString"
            [ test "constructs the correct string from a list of two operations in the correct order" <|
                \_ ->
                    let
                        crdt =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 7 ], char = 'i', isTomb = False }
                                ]
                            }
                    in
                    Expect.equal (CRDT.toString crdt) "Hi"
            , test "constructs the correct string from a list of two operations even if the order is not correct" <|
                \_ ->
                    let
                        crdt =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 7 ], char = 'i', isTomb = False }
                                , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                ]
                            }
                    in
                    Expect.equal (CRDT.toString crdt) "Hi"
            , test "removes characters that are marked as removed" <|
                \_ ->
                    let
                        crdt =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 7 ], char = 'i', isTomb = True }
                                , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                ]
                            }
                    in
                    Expect.equal (CRDT.toString crdt) "H"
            , todo "constructs a to be resolved string if different users edited the same position"

            --, test "constructs a to be resolved string if different users edited the same position" <|
            --    \_ ->
            --        let
            --            crdt =
            --                {seed = Random.initialSeed 42, operations = [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
            --                , { userId = "bob", path = [ 7 ], char = 'i', isTomb = False }
            --                , { userId = "alice", path = [ 0 ], char = 'H', isTomb = False }
            --                , { userId = "alice", path = [ 7 ], char = 'o', isTomb = False }
            --                ]
            --        in
            --        Expect.equal (CRDT.toString crdt) "Hi"
            , test "constructs the correct string from a list of many operations even if the order is not correct" <|
                \_ ->
                    let
                        crdt =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 7 ], char = 'W', isTomb = False }
                                , { userId = "bob", path = [ 11 ], char = 'L', isTomb = False }
                                , { userId = "bob", path = [ 10 ], char = 'R', isTomb = False }
                                , { userId = "bob", path = [ 4 ], char = 'L', isTomb = False }
                                , { userId = "bob", path = [ 5 ], char = 'O', isTomb = False }
                                , { userId = "bob", path = [ 3 ], char = 'L', isTomb = False }
                                , { userId = "bob", path = [ 2 ], char = 'E', isTomb = False }
                                , { userId = "bob", path = [ 6 ], char = ' ', isTomb = False }
                                , { userId = "bob", path = [ 8 ], char = 'O', isTomb = False }
                                , { userId = "bob", path = [ 13 ], char = 'D', isTomb = False }
                                ]
                            }
                    in
                    Expect.equal (CRDT.toString crdt) "HELLO WORLD"
            ]
        , describe "CRDT.update"
            [ test "it adds a character to the end if the updated version differs by one character" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 7 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to the last position if the updated version differs by two characters" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hell"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 11 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 7 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 0 ], char = 'K', isTomb = False }
                                , { userId = "bob", path = [ 1 ], char = 'n', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Kan"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 0, 10 ], char = 'a', isTomb = False }
                            , { userId = "bob", path = [ 0 ], char = 'K', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'n', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register for a long word" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 0 ], char = 'K', isTomb = False }
                                , { userId = "bob", path = [ 1 ], char = 'n', isTomb = False }
                                , { userId = "bob", path = [ 5 ], char = 'g', isTomb = False }
                                , { userId = "bob", path = [ 11 ], char = 'u', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'r', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'u', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Kanguru"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 0, 10 ], char = 'a', isTomb = False }
                            , { userId = "bob", path = [ 0 ], char = 'K', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'n', isTomb = False }
                            , { userId = "bob", path = [ 5 ], char = 'g', isTomb = False }
                            , { userId = "bob", path = [ 11 ], char = 'u', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'r', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'u', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds characters to a sub-register in front if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "AHe"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 0, 10 ], char = 'A', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-register in front if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "ABHe"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 0, 11 ], char = 'B', isTomb = False }
                            , { userId = "bob", path = [ 0, 10 ], char = 'A', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-register at the end if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hell"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 14, 11 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-sub-register at the end if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 14, 14 ], char = 'l', isTomb = False }
                                , { userId = "bob", path = [ 14, 10 ], char = 'l', isTomb = False }
                                , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hello"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 14, 14, 10 ], char = 'o', isTomb = False }
                            , { userId = "bob", path = [ 14, 14 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "removes a single character at the end of the string" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'r', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "H"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 14 ], char = 'r', isTomb = True }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "removes multiple characters at the end of the string" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = [ 13 ], char = 'i', isTomb = False }
                                , { userId = "bob", path = [ 14 ], char = 'r', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "H"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = [ 13 ], char = 'i', isTomb = True }
                            , { userId = "bob", path = [ 14 ], char = 'r', isTomb = True }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            ]
        ]
