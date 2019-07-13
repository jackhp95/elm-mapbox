module Maps.Marker exposing
    ( Marker
    , createCustom
    )

{-| Markers are for displaying geographic locations on the map.

@docs Marker


# Create a marker

@docs createCustom

-}

import Html.Styled as Html exposing (Html)
import Maps.Geo
import Maps.Internal.Marker as Marker exposing (Marker(..))


{-| There are currently two types of marker:

  - A default marker
  - A custom HTML marker

-}
type alias Marker msg =
    Marker.Marker msg


{-| Create a custom HTML marker at the given latitude/longitude.
-}
createCustom : Html msg -> Maps.Geo.LatLng -> Marker msg
createCustom =
    Marker.CustomMarker
