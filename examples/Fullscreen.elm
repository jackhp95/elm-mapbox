module FullScreen exposing (Msg(..), defaultSize, init, main, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html
import Html.Events exposing (onInput)
import Maps
import Maps.Map as Map
import Task


type Msg
    = MapMsg (Maps.Msg ())
    | Resize { width : Int, height : Int }


main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init =
    ( Maps.defaultModel
    , Task.attempt (Result.map .scene >> Result.map (\s -> { width = floor s.width, height = floor s.height }) >> Result.withDefault defaultSize >> Resize) Browser.Dom.getViewport
    )


defaultSize =
    { width = 500, height = 500 }


update msg model =
    case msg of
        MapMsg thismsg ->
            Maps.update thismsg model
                |> Tuple.mapSecond (Cmd.map MapMsg)

        Resize size ->
            ( model
                |> Maps.updateMap (Map.setWidth <| toFloat size.width)
                |> Maps.updateMap (Map.setHeight <| toFloat size.height)
            , Cmd.none
            )


subscriptions model =
    Sub.batch
        [ Sub.map MapMsg <| Maps.subscriptions model
        , Browser.Events.onResize (\w h -> Resize { width = w, height = h })
        ]


view model =
    Html.map MapMsg <| Maps.view model
