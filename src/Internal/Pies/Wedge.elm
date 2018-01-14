module Internal.Pies.Wedge exposing (Config, Wedge(..), wedge, view)

import Lines.Color exposing (Color)
import Svg
import Svg.Attributes
import Internal.Coordinate exposing (Point)

type alias Config  =
  { color : Color,
    name : String,
    data : Float }

type Wedge = Wedge (Config)

wedge : Color -> String -> Float -> Wedge
wedge color name value =
  Wedge <| Config color name value


view : Wedge -> Point -> Svg.Svg msg
view (Wedge wedgeConfig) point =
  Svg.svg [] []
