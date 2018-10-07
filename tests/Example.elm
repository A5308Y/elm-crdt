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
            , test "constructs the correct string from a list of many operations even if the order is not correct" <|
                \_ ->
                    let
                        crdt =
                            [ Insert "andy" [ 0 ] 'H'
                            , Insert "andy" [ 6 ] 'W'
                            , Insert "andy" [ 11 ] 'L'
                            , Insert "andy" [ 10 ] 'R'
                            , Insert "andy" [ 3 ] 'L'
                            , Insert "andy" [ 4 ] 'O'
                            , Insert "andy" [ 2 ] 'L'
                            , Insert "andy" [ 1 ] 'E'
                            , Insert "andy" [ 5 ] ' '
                            , Insert "andy" [ 8 ] 'O'
                            , Insert "andy" [ 13 ] 'D'
                            ]
                    in
                    Expect.equal (CRDT.toString crdt) "HELLO WORLD"
            ]
        ]
