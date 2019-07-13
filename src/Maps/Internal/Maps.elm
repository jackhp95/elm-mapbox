module Maps.Internal.Maps exposing
    ( Msg(..)
    , Model
    , update
    , subscriptions
    , view
    , defaultModel, mapView, updateMap, updateMarkers
    )

{-| The Maps library contains the functions neccessary for an
[HTML.program](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#program).


# Creating a map

The quickest way to get up and running is to create a map with default options

    import Html.Styled as Html exposing (program)
    import Maps.Internal

    main =
        program <| Maps.map Maps.defaultOptions

@docs map
@docs Options
@docs defaultOptions


# Definitions

@docs Msg
@docs Model


# Program functions

@docs update
@docs subscriptions
@docs view

-}

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Html.Styled.Keyed as K
import Json.Decode as Json
import List.Extra as List
import Maps.Internal.Bounds as Bounds exposing (Bounds)
import Maps.Internal.Drag as Drag exposing (Drag)
import Maps.Internal.LatLng as LatLng exposing (LatLng)
import Maps.Internal.Map as Map exposing (Map)
import Maps.Internal.Marker as Marker exposing (Marker)
import Maps.Internal.Pinch as Pinch exposing (Pinch)
import Maps.Internal.Screen as Screen exposing (Offset, TwoFingers, ZoomLevel)
import Maps.Internal.Tile as Tile exposing (Tile)
import Maps.Internal.Utils exposing (flip)
import Maps.Internal.Zoom as Zoom
import Palit as P exposing (..)


{-| The map has events for dragging, zooming and setting the bounds displayed by the map.
-}
type Msg msg
    = DragStart Offset
    | DragTo Offset
    | DragStop
    | PinchStart TwoFingers
    | PinchTo TwoFingers
    | PinchStop
    | Zoom Offset ZoomLevel
    | ExternalMsg msg


{-| The map's model consists of the [properties necessary to display a static map](Maps-Map#Map),
a cache of the previous map (for simulated zooming/panning before the real tiles load in)
and the state of the map being dragged.
-}
type alias Model msg =
    { map : Map
    , cache : List Map
    , markers : List (Marker msg)
    , drag : Maybe Drag
    , pinch : Maybe Pinch
    }


updateMap : (Map -> Map) -> Model msg -> Model msg
updateMap thisupdate model =
    { model
        | map = thisupdate model.map
        , cache = model.map :: model.cache |> List.uniqueBy (.zoom >> ceiling)
    }


updateMarkers : (List (Marker msg) -> List (Marker msg)) -> Model msg -> Model msg
updateMarkers thisupdate model =
    { model
        | markers = thisupdate model.markers
    }


{-| A default model that displays Open Street Map tiles looking at Sydney.
-}
defaultModel : Model msg
defaultModel =
    let
        map =
            { tileServer = "https://a.tile.osm.org/{z}/{x}/{y}.png"
            , zoom = 10
            , center = LatLng.sydney
            , width = 600
            , height = 400
            , tileSize = 256
            }
    in
    { map = map
    , cache = []
    , markers = []
    , drag = Nothing
    , pinch = Nothing
    }


{-| -}
update : Msg msg -> Model msg -> ( Model msg, Cmd (Msg msg) )
update msg model =
    case msg of
        DragStart offset ->
            let
                dragState =
                    if model.pinch == Nothing then
                        Just <| Drag.start offset

                    else
                        Nothing
            in
            ( { model | drag = dragState }, Cmd.none )

        DragTo offset ->
            let
                dragState =
                    if model.pinch == Nothing then
                        Maybe.map (Drag.drag offset) model.drag

                    else
                        Nothing

                draggedMap map =
                    dragState
                        |> Maybe.map (flip Map.drag <| map)
                        |> Maybe.withDefault map
            in
            ( updateMap draggedMap { model | drag = dragState }, Cmd.none )

        DragStop ->
            ( { model | drag = Nothing }, Cmd.none )

        PinchStart fingers ->
            ( { model | pinch = Just <| Pinch.start fingers }, Cmd.none )

        PinchTo fingers ->
            ( { model | pinch = Maybe.map (Pinch.pinch fingers) model.pinch }, Cmd.none )

        PinchStop ->
            let
                zoom =
                    Maybe.map (Zoom.fromPinch model.map.width model.map.height) model.pinch

                pinchedMap map =
                    case zoom of
                        Just ( thiszoom, offset ) ->
                            Map.zoomTo thiszoom offset map

                        Nothing ->
                            map
            in
            ( updateMap pinchedMap { model | pinch = Nothing }, Cmd.none )

        Zoom offset zoom ->
            ( updateMap (Map.zoomTo zoom offset) model, Cmd.none )

        ExternalMsg othermsg ->
            ( model, Cmd.none )


{-| -}
subscriptions : Model msg -> Sub (Msg msg)
subscriptions map =
    Sub.none


{-| -}
view : Model msg -> Html (Msg msg)
view ({ map, cache, markers, pinch, drag } as model) =
    let
        zoom =
            Maybe.map (Zoom.fromPinch map.width map.height) pinch

        zoomedMap =
            Maybe.map (\( thiszoom, offset ) -> Map.zoomTo thiszoom offset map) zoom
                |> Maybe.withDefault map

        transforms =
            Map.diff zoomedMap map

        userSelectNone =
            [ P.style "-webkit-touch-callout" "none"
            , P.style "-webkit-user-select" "none"
            , P.style "-khtml-user-select" "none"
            , P.style "-moz-user-select" "none"
            , P.style "-ms-user-select" "none"
            , P.style "user-select" "none"
            ]

        toPx =
            String.fromFloat >> (\n -> n ++ "px")

        containerView =
            Html.figure
                ([ palit
                    [ reg userSelectNone
                    , bg <| co 0.2 black
                    , P.style "height" (toPx map.height)
                    , P.style "width" (toPx map.width)
                    ]
                 ]
                    ++ zoomEvents map.zoom
                )

        markersView =
            markers
                |> List.map (Marker.view zoomedMap >> Html.map ExternalMsg)
                |> Html.menu
                    [ palit
                        [ P.style "position" "absolute"
                        , P.style "width" <| toPx map.width
                        , P.style "height" <| toPx map.height
                        , P.style "overflow" "hidden"
                        , P.style "pointer-events" "none"
                        , reg userSelectNone
                        ]
                    ]

        cacheView =
            cache
                |> List.foldl (\cachedMap -> (++) (tilesView (Map.diff zoomedMap cachedMap) cachedMap)) []
                |> K.node "div"
                    [ Attr.id "cache"
                    , palit
                        [ absolute
                        , P.style "width" <| toPx map.width
                        , P.style "height" <| toPx map.height
                        , overflowHidden
                        ]
                    , Json.fail "No interaction"
                        |> Json.map (\v -> { message = v, preventDefault = True, stopPropagation = False })
                        |> E.custom "mouseDown"
                    ]

        mapTilesView =
            tilesView transforms map
                |> K.node "div"
                    ([ Attr.id "tiles"
                     , palit
                        [ absolute
                        , P.style "width" <| toPx map.width
                        , P.style "height" <| toPx map.height
                        , overflowHidden
                        , reg userSelectNone
                        ]
                     ]
                        ++ dragEvents drag
                    )
    in
    containerView
        [ cacheView
        , mapTilesView
        , markersView
        ]


{-| Map a Map HTML view to an arbitrary HTML view which wraps map messages.
-}
mapView : (Msg msg -> msg) -> Html (Msg msg) -> Html msg
mapView wrapMsg html =
    let
        mapMsg mapsMsg =
            case mapsMsg of
                ExternalMsg msg ->
                    msg

                thismapsMsg ->
                    wrapMsg thismapsMsg
    in
    Html.map mapMsg html


tilesView : List Map.Transformation -> Map -> List ( String, Html msg )
tilesView transforms map =
    Map.tiles map
        |> List.foldl
            (\tile -> (::) ( Tuple.first tile, Tile.view map.tileSize tile ))
            []



-- |> "div"
--     (transforms
--         |> Map.transformationStyle map.width map.height
--         |> List.map (\( p, v ) -> Attr.style p v)
--     )


zoomEvents : ZoomLevel -> List (Html.Attribute (Msg msg))
zoomEvents zoom =
    Zoom.events
        { zoom = Zoom
        , pinchStart = PinchStart
        , pinchTo = PinchTo
        , pinchStop = PinchStop
        }
        zoom


dragEvents : Maybe Drag -> List (Html.Attribute (Msg msg))
dragEvents drag =
    Drag.events
        { dragStart = DragStart
        , dragTo = DragTo
        , dragStop = DragStop
        }
        drag
