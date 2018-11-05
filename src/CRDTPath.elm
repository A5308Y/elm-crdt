module CRDTPath exposing
    ( CRDTPath
    , absoluteInfimum
    , absoluteSupremum
    , allDifferent
    , choosePathBetween
    , demoPath
    , encode
    , findPathExcluding
    , isBetween
    , sortOrder
    )

import Json.Encode
import Random
import Set exposing (Set)


type CRDTPath
    = CRDTPath (List Int)


crdtRegisterMaximum : Int
crdtRegisterMaximum =
    15


crdtRegisterMinimum : Int
crdtRegisterMinimum =
    0


absoluteInfimum : CRDTPath
absoluteInfimum =
    CRDTPath [ crdtRegisterMinimum ]


absoluteSupremum : CRDTPath
absoluteSupremum =
    CRDTPath [ crdtRegisterMaximum ]


isBetween : CRDTPath -> CRDTPath -> CRDTPath -> Bool
isBetween (CRDTPath infimumPath) (CRDTPath supremumPath) (CRDTPath path) =
    path > infimumPath && path < supremumPath


sortOrder : CRDTPath -> List Int
sortOrder (CRDTPath path) =
    path


values : CRDTPath -> List Int
values (CRDTPath path) =
    path


demoPath : List Int -> CRDTPath
demoPath possiblePath =
    CRDTPath possiblePath


choosePathBetween : Random.Seed -> CRDTPath -> CRDTPath -> ( CRDTPath, Random.Seed )
choosePathBetween seed (CRDTPath infimumPath) (CRDTPath supremumPath) =
    case infimumPath of
        infimumHead :: infimumTail ->
            case supremumPath of
                supremumHead :: supremumTail ->
                    if infimumHead + 1 >= supremumHead then
                        let
                            ( CRDTPath path, nextSeed ) =
                                choosePathBetween seed (CRDTPath infimumTail) (CRDTPath supremumTail)
                        in
                        ( CRDTPath (infimumHead :: path), nextSeed )

                    else
                        nextBetweenStep seed infimumHead supremumHead

                [] ->
                    choosePathBetween seed (CRDTPath infimumPath) (CRDTPath [ crdtRegisterMaximum ])

        [] ->
            case supremumPath of
                supremumHead :: supremumTail ->
                    nextBetweenStep seed crdtRegisterMinimum supremumHead

                [] ->
                    nextBetweenStep seed crdtRegisterMinimum crdtRegisterMaximum


nextBetweenStep : Random.Seed -> Int -> Int -> ( CRDTPath, Random.Seed )
nextBetweenStep seed infimum supremum =
    let
        ( randomInt, nextSeed ) =
            Random.step (Random.int (infimum + 1) (supremum - 1)) seed
    in
    ( CRDTPath [ randomInt ], nextSeed )


increment : CRDTPath -> CRDTPath -> CRDTPath
increment (CRDTPath path) (CRDTPath supremumPath) =
    case path of
        position :: rest ->
            case supremumPath of
                supremumPosition :: supremumRest ->
                    if position + 1 < supremumPosition then
                        CRDTPath ((position + 1) :: rest)

                    else
                        CRDTPath (List.append path [ 1 ])

                [] ->
                    CRDTPath path

        [] ->
            CRDTPath path


findPathExcluding : CRDTPath -> CRDTPath -> List CRDTPath -> CRDTPath
findPathExcluding infimumPath supremumPath excludedPaths =
    if List.member (increment infimumPath supremumPath) excludedPaths then
        findPathExcluding (increment infimumPath supremumPath) supremumPath excludedPaths

    else
        infimumPath


allDifferent : List CRDTPath -> Bool
allDifferent paths =
    List.length paths == Set.size (Set.fromList (List.map values paths))


encode : CRDTPath -> Json.Encode.Value
encode (CRDTPath path) =
    Json.Encode.list Json.Encode.int path
