module Util exposing (isJust, toMaybe)


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


toMaybe : Bool -> a -> Maybe a
toMaybe test just =
    if test then
        Just just

    else
        Nothing
