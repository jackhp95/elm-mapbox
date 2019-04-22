module Maps.Internal.Drag exposing
    ( Drag
    , EventOptions
    , drag
    , events
    , offset
    , start
    )

import Html
import Html.Events exposing (on, onMouseUp)
import Json.Decode as Json
import Maps.Internal.Screen as Screen


type Drag
    = StartDrag Screen.Offset
    | Drag Screen.Offset Screen.Offset


type alias EventOptions msg =
    { dragStart : Screen.Offset -> msg
    , dragTo : Screen.Offset -> msg
    , dragStop : msg
    }


start : Screen.Offset -> Drag
start =
    StartDrag


drag : Screen.Offset -> Drag -> Drag
drag thisoffset state =
    case state of
        StartDrag thisstart ->
            Drag thisstart thisoffset

        Drag thisstart thisend ->
            Drag thisend thisoffset


offset : Drag -> Screen.Offset
offset thisdrag =
    case thisdrag of
        StartDrag _ ->
            { x = 0, y = 0 }

        Drag thisstart thisend ->
            { x = thisend.x - thisstart.x, y = thisend.y - thisstart.y }


events : EventOptions msg -> Maybe Drag -> List (Html.Attribute msg)
events { dragStart, dragTo, dragStop } thisdrag =
    [ -- Mouse
      if thisdrag == Nothing then
        Html.Events.preventDefaultOn "mousedown" <|
            Json.map2 (\b a -> ( a, b )) (Json.succeed True) <|
                Json.map dragStart <|
                    Screen.decodeOffset

      else
        on "mousemove" <|
            Json.map dragTo <|
                Screen.decodeOffset
    , -- Mouse
      onMouseUp dragStop
    , -- Mobile
      if thisdrag == Nothing then
        Html.Events.preventDefaultOn "touchstart" <|
            Json.map2 (\b a -> ( a, b )) (Json.succeed True) <|
                Json.map dragStart <|
                    Screen.decodeOffset

      else
        Html.Events.preventDefaultOn "touchmove" <|
            Json.map2 (\b a -> ( a, b )) (Json.succeed True) <|
                Json.map dragTo <|
                    Screen.decodeOffset
    , -- Mobile
      Html.Events.preventDefaultOn "touchend" <|
        Json.map2 (\b a -> ( a, b )) (Json.succeed True) <|
            Json.succeed dragStop
    ]
