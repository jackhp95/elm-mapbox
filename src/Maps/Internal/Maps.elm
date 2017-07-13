module Maps.Internal.Maps exposing
  ( Msg(..)
  , Model
  , updateMap
  , updateMarkers
  , defaultModel
  , update
  , subscriptions
  , view
  )

{-| The Maps library contains the functions neccessary for an
[HTML.program](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#program).

# Creating a map
The quickest way to get up and running is to create a map with default options

    import Maps.Internal
    import Html exposing (program)

    main = program <| Maps.map Maps.defaultOptions

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

import List.Extra as List

import Json.Decode as Json

import Html exposing (Html, program)
import Html.Keyed
import Html.Attributes as Attr
import Html.Events exposing (onWithOptions)

import Maps.Internal.Map as Map exposing (Map)
import Maps.Internal.Screen as Screen exposing (Offset, TwoFingers, ZoomLevel)
import Maps.Internal.LatLng as LatLng exposing (LatLng)
import Maps.Internal.Bounds as Bounds exposing (Bounds)
import Maps.Internal.Marker as Marker exposing (Marker)
import Maps.Internal.Tile as Tile exposing (Tile)
import Maps.Internal.Drag as Drag exposing (Drag)
import Maps.Internal.Pinch as Pinch exposing (Pinch)
import Maps.Internal.Zoom as Zoom

{-| The map has events for dragging, zooming and setting the bounds displayed by the map.
-}
type Msg
  = DragStart Offset
  | DragTo Offset
  | DragStop
  | PinchStart TwoFingers
  | PinchTo TwoFingers
  | PinchStop
  | Zoom Offset ZoomLevel
  | SetBounds Bounds
  | AddMarker Marker
  | RemoveMarker Marker
  | SetMarkers (List Marker)

{-| The map's model consists of the [properties necessary to display a static map](Maps-Map#Map),
a cache of the previous map (for simulated zooming/panning before the real tiles load in)
and the state of the map being dragged.
-}
type alias Model =
  { map : Map
  , cache : List Map
  , markers : List Marker
  , drag : Maybe Drag
  , pinch : Maybe Pinch
  }

updateMap : (Map -> Map) -> Model -> Model
updateMap update model =
  { model
  | map = update model.map
  , cache = model.map :: model.cache |> List.uniqueBy (.zoom >> ceiling)
  }

updateMarkers : (List Marker -> List Marker) -> Model -> Model
updateMarkers update model =
  { model
  | markers = update model.markers
  , cache = model.map :: model.cache |> List.uniqueBy (.zoom >> ceiling)
  }

{-| A default model that displays Open Street Map tiles looking at Sydney.
-}
defaultModel : Model
defaultModel =
  let
    map =
      { tileServer = "http://a.tile.osm.org/{z}/{x}/{y}.png"
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
update : Msg -> Model -> (Model, Cmd Msg)
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
         ({ model | drag = dragState }, Cmd.none)
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
        (updateMap draggedMap { model | drag = dragState }, Cmd.none)
    DragStop ->
      ({ model | drag = Nothing }, Cmd.none)
    PinchStart fingers ->
      ({ model | pinch = Just <| Pinch.start fingers }, Cmd.none)
    PinchTo fingers ->
      ({ model | pinch = Maybe.map (Pinch.pinch fingers) model.pinch }, Cmd.none)
    PinchStop ->
      let
        zoom = Maybe.map (Zoom.fromPinch model.map.width model.map.height) model.pinch
        pinchedMap map =
          case zoom of
            Just (zoom, offset) -> Map.zoomTo zoom offset map
            Nothing -> map
      in
        (updateMap pinchedMap { model | pinch = Nothing }, Cmd.none)
    Zoom offset zoom ->
      (updateMap (Map.zoomTo zoom offset) model, Cmd.none)
    SetBounds bounds ->
      (updateMap (Map.viewBounds bounds) model, Cmd.none)
    AddMarker marker ->
      ({ model | markers = marker :: model.markers }, Cmd.none)
    RemoveMarker marker ->
      ({ model | markers = List.remove marker model.markers }, Cmd.none)
    SetMarkers markers ->
      ({ model | markers = markers }, Cmd.none)

{-| -}
subscriptions : Model -> Sub Msg
subscriptions map =
  Sub.none

{-| -}
view : Model -> Html Msg
view ({map, cache, markers, pinch, drag} as model) =
  Html.div
    ([ Attr.style
      [ ("width", toString map.width ++ "px")
      , ("height", toString map.height ++ "px")
      , ("background-color", "#ddd")
      ]
    ]
    ++ zoomEvents map.zoom
    )
    <|
    let
      zoom = Maybe.map (Zoom.fromPinch map.width map.height) pinch
      zoomedMap = Maybe.withDefault map <| Maybe.map (\(zoom, offset) -> Map.zoomTo zoom offset map) zoom
      transforms = Map.diff zoomedMap map
    in
      [ Html.div
        [ Attr.style
          [ ("position", "absolute")
          , ("width", toString map.width ++ "px")
          , ("height", toString map.height ++ "px")
          , ("overflow", "hidden")
          ]
        , onWithOptions "mouseDown"
          { preventDefault = True, stopPropagation = False }
          <| Json.fail "No interaction"
        ]
        <| List.map (\cachedMap -> tilesView (Map.diff zoomedMap cachedMap) cachedMap)
        <| List.reverse
        <| cache
      , Html.div
        ([ Attr.style
          [ ("position", "absolute")
          , ("width", toString map.width ++ "px")
          , ("height", toString map.height ++ "px")
          , ("overflow", "hidden")
          ]
        ] ++ dragEvents drag
        )
        [ tilesView transforms map
        ]
      , Html.div
        [ Attr.style
          [ ("position", "absolute")
          , ("width", toString map.width ++ "px")
          , ("height", toString map.height ++ "px")
          , ("overflow", "hidden")
          , ("pointer-events", "none")
          ]
        ]
        <| List.map (Marker.view zoomedMap)
        <| markers
      ]

tilesView : List Map.Transformation -> Map -> Html Msg
tilesView transforms map =
  Html.Keyed.node "div"
    [ Attr.style <| Map.transformationStyle map.width map.height <| transforms ]
    <| List.map (\((url, offset) as tile) -> (url, Tile.view map.tileSize tile))
    <| Map.tiles map

zoomEvents : ZoomLevel -> List (Html.Attribute Msg)
zoomEvents zoom =
  Zoom.events { zoom = Zoom, pinchStart = PinchStart, pinchTo = PinchTo, pinchStop = PinchStop } zoom

dragEvents : Maybe Drag -> List (Html.Attribute Msg)
dragEvents drag =
  Drag.events { dragStart = DragStart, dragTo = DragTo, dragStop = DragStop } drag