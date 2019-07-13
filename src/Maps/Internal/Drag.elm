module Maps.Internal.Drag exposing
    ( Drag
    , EventOptions
    , drag
    , events
    , offset
    , start
    )

import Html.Styled as Html
import Html.Styled.Events as E exposing (on, onMouseUp)
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
        Screen.decodeOffset
            |> Json.map dragStart
            |> Json.map2 (\b a -> ( a, b )) (Json.succeed True)
            |> E.preventDefaultOn "mousedown"

      else
        Screen.decodeOffset
            |> Json.map dragTo
            |> on "mousemove"
    , -- Mouse
      onMouseUp dragStop
    , -- Mobile
      if thisdrag == Nothing then
        Screen.decodeOffset
            |> Json.map dragStart
            -- |> Json.map2 (\b a -> ( a, b )) (Json.succeed True)
            |> E.on "touchstart"

      else
        Screen.decodeOffset
            |> Json.map dragTo
            -- |> Json.map2 (\b a -> ( a, b )) (Json.succeed True)
            |> E.on "touchmove"
    , -- Mobile
      Json.succeed dragStop
        -- |> Json.map2 (\b a -> ( a, b )) (Json.succeed True)
        |> E.on "touchend"
    ]
