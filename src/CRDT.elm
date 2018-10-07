module CRDT exposing (Operation(..), demo, toString)


type alias CRDT =
    List Operation


type Operation
    = Insert UserId Path Char


type alias UserId =
    String


type alias Path =
    List Int



--Can't be a set because Insert is not comparable.


demo =
    [ Insert "andy" [ 0 ] 'H'
    , Insert "andy" [ 1 ] 'E'
    , Insert "andy" [ 5 ] ' '
    , Insert "andy" [ 2 ] 'L'
    , Insert "andy" [ 10 ] 'R'
    , Insert "andy" [ 3 ] 'L'
    , Insert "andy" [ 8 ] 'O'
    , Insert "andy" [ 4 ] 'O'
    , Insert "andy" [ 6 ] 'W'
    , Insert "andy" [ 11 ] 'L'
    , Insert "andy" [ 13 ] 'D'
    ]


toString : CRDT -> String
toString crdt =
    crdt
        |> List.sortBy operationOrder
        |> List.map displayInsert
        |> String.concat


displayInsert : Operation -> String
displayInsert operation =
    case operation of
        Insert userId path char ->
            String.fromChar char


operationOrder crdt =
    case crdt of
        Insert _ path _ ->
            path
