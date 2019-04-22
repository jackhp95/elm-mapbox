module Fuzzers exposing
    ( latLng
    , map
    , noOffset
    , offset
    , zoomLevel
    )

import Fuzz exposing (..)
import Maps.Internal.LatLng exposing (LatLng)
import Maps.Internal.Map exposing (..)
import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Random
import Shrink


offset : Fuzzer Screen.Offset
offset =
    Fuzz.map2 Screen.Offset
        offsetSingle
        offsetSingle


noOffset : Fuzzer Screen.Offset
noOffset =
    constant <| Screen.Offset 0 0


offsetSingle : Fuzzer Float
offsetSingle =
    signedRange -5000 5000


map : Fuzzer Map
map =
    Fuzz.map5 (Map "http://a.tile.osm.org/{z}/{x}/{y}.png")
        zoomLevel
        -- Zoom level
        latLng
        -- Map center
        (constant 500)
        -- Width
        (constant 500)
        -- Height
        (constant 256)



-- Tile size


zoomLevel : Fuzzer ZoomLevel
zoomLevel =
    Fuzz.map toFloat <|
        intRange 1 20


latLng : Fuzzer LatLng
latLng =
    Fuzz.map2 LatLng
        lat
        lng


lat : Fuzzer Float
lat =
    signedRange -90 90


lng : Fuzzer Float
lng =
    signedRange -180 180


signedRange min max =
    Fuzz.floatRange min max



{--
Old signedRange implementation
  Fuzz.custom
  (Random.float min max)
  (shrinkSignedRange min max)
--}


shrinkSignedRange min max val =
    if val == min || val == max || val == 0 then
        []

    else if val < min then
        min :: []

    else if val > max then
        max :: []

    else if Basics.toFloat (floor val) /= val || Basics.toFloat (ceiling val) /= val then
        (toFloat <| floor val) :: 0 :: []

    else if val < 0 then
        (toFloat <| floor val // 2) :: []

    else
        []
