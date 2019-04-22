module Maps.Internal.Pinch exposing
    ( Pinch
    , pinch
    , start
    , startEnd
    )

import Maps.Internal.Screen as Screen exposing (TwoFingers)


type Pinch
    = StartPinch TwoFingers
    | Pinch TwoFingers TwoFingers


start : TwoFingers -> Pinch
start =
    StartPinch


pinch : TwoFingers -> Pinch -> Pinch
pinch twoFingers state =
    case state of
        StartPinch thisstart ->
            Pinch thisstart twoFingers

        Pinch thisstart end ->
            Pinch thisstart twoFingers


startEnd : Pinch -> ( TwoFingers, TwoFingers )
startEnd thispinch =
    case thispinch of
        StartPinch thisstart ->
            ( thisstart, thisstart )

        Pinch thisstart end ->
            ( thisstart, end )
