module Maps.Internal.Utils exposing
    ( cartesianMap
    , flip
    , sinh
    , wrap
    )


wrap min max n =
    if n < min then
        wrap min max <| n + (max - min)

    else if n >= max then
        wrap min max <| n - (max - min)

    else
        n


sinh x =
    ((e ^ x) - (e ^ -x)) / 2


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


cartesianMap : (a -> b -> c) -> List a -> List b -> List (List c)
cartesianMap function rows columns =
    flip function
        |> flip List.map columns
        |> List.map (flip List.map rows)
