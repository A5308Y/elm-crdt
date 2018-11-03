module CRDTPath exposing
    ( CRDTPath
    , absoluteInfimum
    , absoluteSupremum
    , choosePathBetween
    , demoPath
    , isBetween
    , sortOrder
    )

import Random


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
