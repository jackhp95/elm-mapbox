module Maps.Internal.Marker exposing
    ( Marker(..)
    , view
    )

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Maps.Internal.LatLng as LatLng exposing (LatLng)
import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Palit as P exposing (..)


type Marker msg
    = CustomMarker (Html msg) LatLng


view : { a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng } -> Marker msg -> Html msg
view map marker =
    case marker of
        CustomMarker html latLng ->
            let
                offset =
                    Screen.offsetFromLatLng map latLng

                translateXY =
                    (\( x, y ) ->
                        [ ( "-webkit-transform", "translateX(" ++ x ++ ") translateY(" ++ y ++ ")" )
                        , ( "-moz-transform", "translateX(" ++ x ++ ") translateY(" ++ y ++ ")" )
                        , ( "transform", "translateX(" ++ x ++ ") translateY(" ++ y ++ ")" )
                        ]
                    )
                        ( String.fromFloat offset.x ++ "px"
                        , String.fromFloat offset.y ++ "px"
                        )
            in
            Html.li
                (List.map
                    (\( p, v ) -> Attr.style p v)
                    ([ ( "position", "absolute" )
                     , ( "left", "0" )
                     , ( "top", "0" )
                     , ( "max-height", "0" )
                     , ( "max-width", "0" )
                     , ( "height", "0" )
                     , ( "width", "0" )
                     , ( "pointer-events", "initial" )
                     ]
                        ++ translateXY
                    )
                    ++ [ palit [ flex, itemsCenter, justifyCenter ] ]
                )
                [ html ]
