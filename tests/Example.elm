module Example exposing (suite)

import CRDT exposing (Operation(..))
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
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 7 ] 'i' False
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
                                [ Insert "bob" [ 7 ] 'i' False
                                , Insert "bob" [ 1 ] 'H' False
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
                                [ Insert "bob" [ 7 ] 'i' True
                                , Insert "bob" [ 1 ] 'H' False
                                ]
                            }
                    in
                    Expect.equal (CRDT.toString crdt) "H"
            , todo "constructs a to be resolved string if different users edited the same position"

            --, test "constructs a to be resolved string if different users edited the same position" <|
            --    \_ ->
            --        let
            --            crdt =
            --                {seed = Random.initialSeed 42, operations = [ Insert "bob" [ 1 ] 'H' False
            --                , Insert "bob" [ 7 ] 'i' False
            --                , Insert "alice" [ 0 ] 'H' False
            --                , Insert "alice" [ 7 ] 'o' False
            --                ]
            --        in
            --        Expect.equal (CRDT.toString crdt) "Hi"
            , test "constructs the correct string from a list of many operations even if the order is not correct" <|
                \_ ->
                    let
                        crdt =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 7 ] 'W' False
                                , Insert "bob" [ 11 ] 'L' False
                                , Insert "bob" [ 10 ] 'R' False
                                , Insert "bob" [ 4 ] 'L' False
                                , Insert "bob" [ 5 ] 'O' False
                                , Insert "bob" [ 3 ] 'L' False
                                , Insert "bob" [ 2 ] 'E' False
                                , Insert "bob" [ 6 ] ' ' False
                                , Insert "bob" [ 8 ] 'O' False
                                , Insert "bob" [ 13 ] 'D' False
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
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 7 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 10 ] 'l' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 7 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to the last position if the updated version differs by two characters" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 7 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "Hell"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 11 ] 'l' False
                            , Insert "bob" [ 10 ] 'l' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 7 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 14 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 14, 10 ] 'l' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 14 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 0 ] 'K' False
                                , Insert "bob" [ 1 ] 'n' False
                                ]
                            }
                                |> CRDT.update "bob" "Kan"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 0, 10 ] 'a' False
                            , Insert "bob" [ 0 ] 'K' False
                            , Insert "bob" [ 1 ] 'n' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register for a long word" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 0 ] 'K' False
                                , Insert "bob" [ 1 ] 'n' False
                                , Insert "bob" [ 5 ] 'g' False
                                , Insert "bob" [ 11 ] 'u' False
                                , Insert "bob" [ 14 ] 'r' False
                                , Insert "bob" [ 14 ] 'u' False
                                ]
                            }
                                |> CRDT.update "bob" "Kanguru"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 0, 10 ] 'a' False
                            , Insert "bob" [ 0 ] 'K' False
                            , Insert "bob" [ 1 ] 'n' False
                            , Insert "bob" [ 5 ] 'g' False
                            , Insert "bob" [ 11 ] 'u' False
                            , Insert "bob" [ 14 ] 'r' False
                            , Insert "bob" [ 14 ] 'u' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 14 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "Hel"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 14, 10 ] 'l' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 14 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds characters to a sub-register in front if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 14 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "AHe"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 0, 10 ] 'A' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 14 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-register in front if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 14 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "ABHe"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 0, 11 ] 'B' False
                            , Insert "bob" [ 0, 10 ] 'A' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 14 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-register at the end if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 14 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "Hell"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 14, 11 ] 'l' False
                            , Insert "bob" [ 14, 10 ] 'l' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 14 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds multiple characters to a sub-sub-register at the end if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 14, 14 ] 'l' False
                                , Insert "bob" [ 14, 10 ] 'l' False
                                , Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 14 ] 'e' False
                                ]
                            }
                                |> CRDT.update "bob" "Hello"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 14, 14, 10 ] 'o' False
                            , Insert "bob" [ 14, 14 ] 'l' False
                            , Insert "bob" [ 14, 10 ] 'l' False
                            , Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 14 ] 'e' False
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "removes a single character at the end of the string" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 14 ] 'r' False
                                ]
                            }
                                |> CRDT.update "bob" "H"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 14 ] 'r' True
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "removes multiple characters at the end of the string" <|
                \_ ->
                    let
                        calculatedResult =
                            { seed = Random.initialSeed 42
                            , operations =
                                [ Insert "bob" [ 1 ] 'H' False
                                , Insert "bob" [ 13 ] 'i' False
                                , Insert "bob" [ 14 ] 'r' False
                                ]
                            }
                                |> CRDT.update "bob" "H"
                                |> .operations

                        expectedResult =
                            [ Insert "bob" [ 1 ] 'H' False
                            , Insert "bob" [ 13 ] 'i' True
                            , Insert "bob" [ 14 ] 'r' True
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            ]
        ]
