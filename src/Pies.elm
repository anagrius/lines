module Pies exposing (view, wedge)

import Internal.Pies.Wedge exposing (Wedge(..))
import Lines.Color exposing (Color)
import Lines.Coordinate as Coordinate
import Svg exposing (Svg)
import Svg.Attributes as Attr


--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


type alias Config data msg =
    { x : data, y : msg, width : Int, height : Int }


type alias Wedge =
    Internal.Pies.Wedge.Wedge



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


px : Int -> String
px v =
    toString v ++ "px"


view : Config data msg -> List Wedge -> Svg msg
view config wedges =
    let
        dataPoints =
            toPercent wedges
    in
    Svg.svg
        [ Attr.viewBox "-1 -1 2 2"
        , Attr.width (px config.width)
        , Attr.height (px config.height)
        , Attr.style "transform: rotate(-0.25turn)"
        ]
        (viewWedges [] 0.0 dataPoints)


viewWedges : List (Svg msg) -> Float -> List DataPoint -> List (Svg msg)
viewWedges result sumSoFar remainingPoints =
    case remainingPoints of
        [] ->
            List.reverse result

        p :: rest ->
            let
                newSumSoFar =
                    sumSoFar + p.percent

                w =
                    viewWedge sumSoFar p
            in
            viewWedges (w :: result) newSumSoFar rest


viewWedge : Float -> DataPoint -> Svg msg
viewWedge sumSoFar { percent, wedge } =
    let
        (Wedge { color, name }) =
            wedge

        arcStart =
            getCoordinatesForPercent sumSoFar

        arcEnd =
            getCoordinatesForPercent (percent + sumSoFar)

        largeArcFlag =
            if percent > 0.5 then
                "1"
            else
                "0"
    in
    Svg.path [ Attr.d <| "M " ++ toString arcStart.x ++ " " ++ toString arcStart.y ++ " A 1 1 0 " ++ largeArcFlag ++ " 1 " ++ toString arcEnd.x ++ " " ++ toString arcEnd.y ++ " L 0 0", Attr.fill color ] [ Svg.text (toString percent) ]


toPercent : List Wedge -> List DataPoint
toPercent wedges =
    let
        theSum =
            List.sum (List.map (\(Wedge { data }) -> data) wedges)
    in
    List.map (\(Wedge w) -> { wedge = Wedge w, percent = w.data / theSum }) wedges


type alias DataPoint =
    { wedge : Wedge, percent : Float }


wedge : Color -> String -> Float -> Wedge
wedge =
    Internal.Pies.Wedge.wedge


getCoordinatesForPercent : Float -> Coordinate.Point
getCoordinatesForPercent percent =
    { x = cos <| 2 * pi * percent
    , y = sin <| 2 * pi * percent
    }
