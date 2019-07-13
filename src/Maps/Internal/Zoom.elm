module Maps.Internal.Zoom exposing
    ( EventOptions
    , events
    , fromPinch
    )

import Html.Styled as Html
import Html.Styled.Events as E exposing (on)
import Json.Decode as Json
import Maps.Internal.Pinch as Pinch exposing (Pinch)
import Maps.Internal.Screen as Screen exposing (ZoomLevel)


type alias EventOptions msg =
    { zoom : Screen.Offset -> ZoomLevel -> msg
    , pinchStart : Screen.TwoFingers -> msg
    , pinchTo : Screen.TwoFingers -> msg
    , pinchStop : msg
    }


fromPinch : Float -> Float -> Pinch -> ( ZoomLevel, Screen.Offset )
fromPinch mapWidth mapHeight pinch =
    let
        ( start, end ) =
            Pinch.startEnd pinch
    in
    ( logBase 2 (end.length / start.length)
    , start.center
    )


events : EventOptions msg -> ZoomLevel -> List (Html.Attribute msg)
events { zoom, pinchStart, pinchTo, pinchStop } mapZoom =
    let
        customEventOn str x =
            Json.map (\v -> { message = v, preventDefault = True, stopPropagation = False }) x
                |> E.custom str
    in
    [ -- Mouse
      Screen.decodeOffset
        |> Json.map (\offset -> zoom offset 1)
        |> customEventOn "dblclick"
    , --Mouse
      Json.map2
        zoom
        Screen.decodeOffset
        Screen.decodeZoom
        |> customEventOn "wheel"
    , -- Mobile
      Screen.decodeTwoFingers
        |> Json.map (Maybe.map pinchStart)
        |> Json.map (Maybe.withDefault pinchStop)
        |> on "touchstart"
    , -- Mobile
      Screen.decodeTwoFingers
        |> Json.map (Maybe.map pinchTo)
        |> Json.map (Maybe.withDefault pinchStop)
        |> customEventOn "touchmove"
    , -- Mobile
      Json.succeed pinchStop
        |> on "touchend"
    ]
