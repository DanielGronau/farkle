module Die exposing
    ( Dice
    , Die
    , applyCombis
    , combinations
    , createDice
    , dieFace
    , dieIndex
    , hasFarkled
    , isMarked
    , markedDice
    , toggleDie
    )

import Util exposing (..)


type alias Points =
    Int


type alias Face =
    Int


type alias Faces =
    List Int


type alias Combination =
    List Face


type Die
    = Die { face : Int, marked : Bool, index : Int }


type alias Dice =
    List Die


createDie : Int -> Face -> Die
createDie index face =
    Die { face = face, marked = False, index = index }


createDice : Faces -> Dice
createDice faces =
    List.indexedMap createDie faces


dieFace : Die -> Face
dieFace (Die die) =
    die.face


isMarked : Die -> Bool
isMarked (Die die) =
    die.marked


dieIndex : Die -> Int
dieIndex (Die die) =
    die.index


toggleDie : Int -> Die -> Die
toggleDie index (Die die) =
    Die { die | marked = (die.index == index) /= die.marked }


markedDice : Dice -> Faces
markedDice =
    List.filter isMarked >> List.map dieFace >> List.sort


applyCombis : Combination -> Maybe Points
applyCombis dice =
    let
        ( remainingDice, points ) =
            List.foldl applyCombi ( dice, 0 ) combinations
    in
    Util.toMaybe (remainingDice == []) points


applyCombi : ( Combination, Points ) -> ( Faces, Points ) -> ( Faces, Points )
applyCombi ( combi, combiPoints ) ( dice, initialPoints ) =
    containsCombi dice combi
        |> Maybe.map (\remainingDice -> ( remainingDice, initialPoints + combiPoints ))
        |> Maybe.withDefault ( dice, initialPoints )


containsCombi : Faces -> Combination -> Maybe Faces
containsCombi dice combi =
    let
        ( remainingCombi, remainingDice ) =
            List.foldl matchCombi ( combi, [] ) dice
    in
    Util.toMaybe (remainingCombi == []) remainingDice


hasFarkled : Faces -> Bool
hasFarkled dice =
    case dice of
        [] ->
            False

        _ ->
            combinations
                |> List.map Tuple.first
                |> List.any (containsCombi dice >> isJust)
                |> not


matchCombi : Int -> ( Combination, Faces ) -> ( Combination, Faces )
matchCombi face ( combi, remainingDice ) =
    if List.take 1 combi == [ face ] then
        ( List.drop 1 combi, remainingDice )

    else
        ( combi, remainingDice ++ [ face ] )


combinations : List ( Combination, Points )
combinations =
    [ ( [ 1, 1, 1, 1, 1, 1 ], 8000 )
    , ( [ 2, 2, 2, 2, 2, 2 ], 1600 )
    , ( [ 3, 3, 3, 3, 3, 3 ], 2400 )
    , ( [ 4, 4, 4, 4, 4, 4 ], 3200 )
    , ( [ 5, 5, 5, 5, 5, 5 ], 4000 )
    , ( [ 6, 6, 6, 6, 6, 6 ], 4800 )
    , ( [ 1, 2, 3, 4, 5, 6 ], 1500 )
    , ( [ 1, 1, 1, 1, 1 ], 4000 )
    , ( [ 2, 2, 2, 2, 2 ], 800 )
    , ( [ 3, 3, 3, 3, 3 ], 1200 )
    , ( [ 4, 4, 4, 4, 4 ], 1600 )
    , ( [ 5, 5, 5, 5, 5 ], 2000 )
    , ( [ 6, 6, 6, 6, 6 ], 2400 )
    , ( [ 1, 2, 3, 4, 5 ], 500 )
    , ( [ 2, 3, 4, 5, 6 ], 500 )
    , ( [ 1, 1, 1, 1 ], 2000 )
    , ( [ 2, 2, 2, 2 ], 400 )
    , ( [ 3, 3, 3, 3 ], 600 )
    , ( [ 4, 4, 4, 4 ], 800 )
    , ( [ 5, 5, 5, 5 ], 1000 )
    , ( [ 6, 6, 6, 6 ], 1200 )
    , ( [ 1, 1, 1 ], 1000 )
    , ( [ 2, 2, 2 ], 200 )
    , ( [ 3, 3, 3 ], 300 )
    , ( [ 4, 4, 4 ], 400 )
    , ( [ 5, 5, 5 ], 500 )
    , ( [ 6, 6, 6 ], 600 )
    , ( [ 1, 1 ], 200 )
    , ( [ 5, 5 ], 100 )
    , ( [ 1 ], 100 )
    , ( [ 5 ], 50 )
    ]
