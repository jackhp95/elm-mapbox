module Maps.Internal.Tile exposing
    ( Offset
    , Tile
    , fromLatLng
    , toLatLng
    , url
    , view
    )

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Maps.Internal.LatLng as LatLng exposing (LatLng)
import Maps.Internal.Utils exposing (sinh, wrap)
import Regex
import Palit as P exposing (..)

type alias Url =
    String


type alias Tile =
    ( Url, Offset )


type alias Offset =
    { x : Float
    , y : Float
    }


url : String -> Int -> Int -> Int -> Url
url tileServer zoom x y =
    tileServer
        |> formatInt "{z}" zoom
        |> formatInt "{x}" x
        |> formatInt "{y}" y


fromLatLng : Float -> LatLng -> Offset
fromLatLng zoom loc =
    let
        n =
            2 ^ zoom

        x =
            n * ((loc.lng + 180) / 360) |> wrap 0 n

        latRad =
            loc.lat * pi / 180

        y =
            n * (1 - (logBase e <| abs <| tan latRad + (1 / cos latRad)) / pi) / 2
    in
    Offset x y


toLatLng : Float -> Float -> Float -> LatLng
toLatLng zoom tileX tileY =
    let
        n =
            2 ^ zoom

        lngDeg =
            tileX / n * 360 - 180 |> wrap -180 180

        latRad =
            atan <| sinh <| pi * (1 - 2 * tileY / n)

        latDeg =
            latRad * 180 / pi
    in
    LatLng latDeg lngDeg


formatInt : String -> Int -> String -> String
formatInt replace number =
    let
        attemptedRegex =
            Regex.fromString replace
    in
    case attemptedRegex of
        Just regex ->
            Regex.replace regex (\_ -> String.fromInt number)

        Nothing ->
            identity


view : Float -> Tile -> Html msg
view tileSize ( thisUrl, offset ) =
    let 
        height = String.fromFloat offset.y ++ "px"
        width = String.fromFloat offset.x ++ "px"
    in
    Html.img
        [ Attr.src thisUrl
        , palit [absolute 
        -- , P.style "left" <| String.fromFloat offset.x ++ "px"
        -- , P.style "top" <| String.fromFloat offset.y ++ "px"
        , P.style "transform" <| String.join " " ["translate3D(", width, ",", height, ",",  "0", ")"]
        , P.style "width" <| String.fromFloat tileSize ++ "px"
        , P.style "height" <| String.fromFloat tileSize ++ "px"]
        ]
        []
