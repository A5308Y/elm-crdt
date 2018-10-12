module Example exposing (suite)

import CRDT exposing (Operation(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "CRDT"
        [ describe "CRDT.toString"
            [ test "constructs the correct string from a list of two operations in the correct order" <|
                \_ ->
                    let
                        crdt =
                            [ Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 7 ] 'i'
                            ]
                    in
                    Expect.equal (CRDT.toString crdt) "Hi"
            , test "constructs the correct string from a list of two operations even if the order is not correct" <|
                \_ ->
                    let
                        crdt =
                            [ Insert "bob" [ 7 ] 'i'
                            , Insert "bob" [ 0 ] 'H'
                            ]
                    in
                    Expect.equal (CRDT.toString crdt) "Hi"

            --, test "constructs a to be resolved string if different users edited the same position" <|
            --    \_ ->
            --        let
            --            crdt =
            --                [ Insert "bob" [ 0 ] 'H'
            --                , Insert "bob" [ 7 ] 'i'
            --                , Insert "alice" [ 0 ] 'H'
            --                , Insert "alice" [ 7 ] 'o'
            --                ]
            --        in
            --        Expect.equal (CRDT.toString crdt) "Hi"
            , test "constructs the correct string from a list of many operations even if the order is not correct" <|
                \_ ->
                    let
                        crdt =
                            [ Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 6 ] 'W'
                            , Insert "bob" [ 11 ] 'L'
                            , Insert "bob" [ 10 ] 'R'
                            , Insert "bob" [ 3 ] 'L'
                            , Insert "bob" [ 4 ] 'O'
                            , Insert "bob" [ 2 ] 'L'
                            , Insert "bob" [ 1 ] 'E'
                            , Insert "bob" [ 5 ] ' '
                            , Insert "bob" [ 8 ] 'O'
                            , Insert "bob" [ 13 ] 'D'
                            ]
                    in
                    Expect.equal (CRDT.toString crdt) "HELLO WORLD"
            ]
        , describe "CRDT.update"
            [ test "it adds a character to the end if the updated version differs by one character" <|
                \_ ->
                    let
                        calculatedResult =
                            [ Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 7 ] 'e'
                            ]
                                |> CRDT.update "bob" "Hel"

                        expectedResult =
                            [ Insert "bob" [ 8 ] 'l'
                            , Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 7 ] 'e'
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to the last position if the updated version differs by two characters" <|
                \_ ->
                    let
                        calculatedResult =
                            [ Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 7 ] 'e'
                            ]
                                |> CRDT.update "bob" "Hell"

                        expectedResult =
                            [ Insert "bob" [ 9 ] 'l'
                            , Insert "bob" [ 8 ] 'l'
                            , Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 7 ] 'e'
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            [ Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 15 ] 'e'
                            ]
                                |> CRDT.update "bob" "Hel"

                        expectedResult =
                            [ Insert "bob" [ 15, 0 ] 'l'
                            , Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 15 ] 'e'
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register" <|
                \_ ->
                    let
                        calculatedResult =
                            [ Insert "bob" [ 0 ] 'K'
                            , Insert "bob" [ 1 ] 'n'
                            ]
                                |> CRDT.update "bob" "Kan"

                        expectedResult =
                            [ Insert "bob" [ 0, 0 ] 'a'
                            , Insert "bob" [ 0 ] 'K'
                            , Insert "bob" [ 1 ] 'n'
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds one character to a sub-register if there is no space between two characters in the parent register for a long word" <|
                \_ ->
                    let
                        calculatedResult =
                            [ Insert "bob" [ 0 ] 'K'
                            , Insert "bob" [ 1 ] 'n'
                            , Insert "bob" [ 5 ] 'g'
                            , Insert "bob" [ 11 ] 'u'
                            , Insert "bob" [ 14 ] 'r'
                            , Insert "bob" [ 15 ] 'u'
                            ]
                                |> CRDT.update "bob" "Kanguru"

                        expectedResult =
                            [ Insert "bob" [ 0, 0 ] 'a'
                            , Insert "bob" [ 0 ] 'K'
                            , Insert "bob" [ 1 ] 'n'
                            , Insert "bob" [ 5 ] 'g'
                            , Insert "bob" [ 11 ] 'u'
                            , Insert "bob" [ 14 ] 'r'
                            , Insert "bob" [ 15 ] 'u'
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "it adds two characters to a sub-register if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            [ Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 15 ] 'e'
                            ]
                                |> CRDT.update "bob" "Hel"

                        expectedResult =
                            [ Insert "bob" [ 15, 0 ] 'l'
                            , Insert "bob" [ 0 ] 'H'
                            , Insert "bob" [ 15 ] 'e'
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            , test "adds characters to a sub-register in front if the parent register is already full" <|
                \_ ->
                    let
                        calculatedResult =
                            [ Insert "bob" [ 1 ] 'H'
                            , Insert "bob" [ 15 ] 'e'
                            ]
                                |> CRDT.update "bob" "AHe"

                        expectedResult =
                            [ Insert "bob" [ 0, 0 ] 'A'
                            , Insert "bob" [ 1 ] 'H'
                            , Insert "bob" [ 15 ] 'e'
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            ]
        ]
