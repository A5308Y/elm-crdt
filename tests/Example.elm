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
            [ test "it adds a character in a correct position if the updated version differs by one character" <|
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
            ]
        , describe "CRDT.zip"
            [ test "it combines to list as an outer join" <|
                \_ ->
                    let
                        calculatedResult =
                            CRDT.zip [ 'H', 'E', 'L' ] [ ( 'H', [ 3 ] ), ( 'E', [ 6 ] ) ]

                        expectedResult =
                            [ ( Just 'H', Just ( 'H', [ 3 ] ) )
                            , ( Just 'E', Just ( 'E', [ 6 ] ) )
                            , ( Just 'L', Nothing )
                            ]
                    in
                    Expect.equal calculatedResult expectedResult
            ]
        ]
