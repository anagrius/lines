module Lines.Axis.Line exposing
  ( Line, none, default
  , full, rangeFrame
  , Config, custom
  )

{-|

# Quick start
@docs default, none

# Configurations
@docs Line, full, rangeFrame

# Customizing
@docs Config, custom

-}

import Svg exposing (Attribute)
import Lines.Color as Color
import Internal.Coordinate as Coordinate
import Internal.Axis.Line as Line


{-| -}
type alias Line msg =
  Line.Line msg


{-| -}
default : Line msg
default =
  Line.default


{-| Removes the axis line entirely.
-}
none : Line msg
none =
  Line.none


{-| Draws the axis line as the full length of your dimension.
-}
full : Line msg
full =
  Line.full


{-| Draws the axis line to fit the range of your data.
-}
rangeFrame : Line msg
rangeFrame =
  Line.rangeFrame



-- CUSTOM


{-| -}
type alias Config msg =
  { color : Color.Color
  , width : Float
  , events : List (Attribute msg)
  , start : Float
  , end : Float
  }


{-| Given the range of your data and your dimension range, define your own
axis line configuration.
-}
custom : (Coordinate.Range -> Coordinate.Range -> Config msg) -> Line msg
custom =
  Line.custom
