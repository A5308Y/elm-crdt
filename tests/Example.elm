module Example exposing (suite)

import CRDT
import CRDTPath
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
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'i', isTomb = False }
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
                                [ { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'i', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
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
                                [ { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'i', isTomb = True }
                                , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                ]
                            }
                    in
                    Expect.equal (CRDT.toString crdt) "H"
            , test "constructs the correct string from a list of many operations even if the order is not correct" <|
                \_ ->
                    let
                        crdt =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'W', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 11 ], char = 'L', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 10 ], char = 'R', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 4 ], char = 'L', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 5 ], char = 'O', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 3 ], char = 'L', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 2 ], char = 'E', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = ' ', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 8 ], char = 'O', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 13 ], char = 'D', isTomb = False }
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
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to the last position if the updated version differs by two characters" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hell"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 11 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 0 ], char = 'K', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'n', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Kan"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 0, 10 ], char = 'a', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 0 ], char = 'K', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'n', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register for a long word" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 0 ], char = 'K', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'n', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 5 ], char = 'g', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 11 ], char = 'u', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'r', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'u', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Kanguru"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 0, 10 ], char = 'a', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 0 ], char = 'K', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'n', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 5 ], char = 'g', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 11 ], char = 'u', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'r', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'u', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds the same character at the end if it gets repeated" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "HH"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 10 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds characters to a sub-register in front if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "AHe"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 0, 10 ], char = 'A', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-register in front if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "ABHe"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 0, 11 ], char = 'B', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 0, 10 ], char = 'A', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-register at the end if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hell"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 14, 11 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-sub-register at the end if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 14, 14 ], char = 'l', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14, 10 ], char = 'l', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hello"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 14, 14, 10 ], char = 'o', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14, 14 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14, 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "removes a single character at the end of the string" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'r', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "H"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'r', isTomb = True }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "removes multiple characters at the end of the string" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 13 ], char = 'i', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'r', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "H"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 13 ], char = 'i', isTomb = True }
                            , { userId = "bob", path = CRDTPath.demoPath [ 14 ], char = 'r', isTomb = True }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it does not allow to override paths" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = True }
                                , { userId = "bob", path = CRDTPath.demoPath [ 8 ], char = 'l', isTomb = False }
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 7, 10 ], char = 'e', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = True }
                            , { userId = "bob", path = CRDTPath.demoPath [ 8 ], char = 'l', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "handles multi-user additions correctly" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "alice" "Hel"
                                |> .operations

                        expectedResult =
                            [ { userId = "alice", path = CRDTPath.demoPath [ 10 ], char = 'l', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "handles multi-user deletions correctly" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "alice" ""
                                |> .operations

                        expectedResult =
                            [ { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = True }
                            , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = True }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "handles multi-user replacements correctly" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.update "alice" "Ha"
                                |> .operations

                        expectedResult =
                            [ { userId = "alice", path = CRDTPath.demoPath [ 10 ], char = 'a', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 6 ], char = 'H', isTomb = False }
                            , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = True }
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            ]
        , describe "isResolved"
            [ test "returns True if there are no conflicting entries" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.isResolved

                        expectedResult =
                            True
                    in
                    Expect.equal calculatedResult expectedResult
            , test "returns False if there are conflicting entries" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ { userId = "bob", path = CRDTPath.demoPath [ 1 ], char = 'H', isTomb = False }
                                , { userId = "bob", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                , { userId = "alice", path = CRDTPath.demoPath [ 7 ], char = 'e', isTomb = False }
                                ]
                            }
                                |> CRDT.isResolved

                        expectedResult =
                            False
                    in
                    Expect.equal calculatedResult expectedResult
            ]
        ]
