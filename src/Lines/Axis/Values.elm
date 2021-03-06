module Lines.Axis.Values exposing (Amount, around, exactly, int, time, float, interval)

{-|

# Values
@docs int, float, time, interval

## Amount
@docs Amount, around, exactly

-}

import Lines.Axis.Tick exposing (Time, Unit(..), Interval)
import Internal.Axis.Values as Values
import Internal.Coordinate as Coordinate


{-| -}
type alias Amount =
  Values.Amount


{-| -}
around : Int -> Amount
around =
  Values.around


{-| -}
exactly : Int -> Amount
exactly =
  Values.exactly



-- NUMBERS


{-| -}
int : Amount -> Coordinate.Range -> List Int
int =
  Values.int


{-| -}
float : Amount -> Coordinate.Range -> List Float
float =
  Values.float


{-| -}
interval : Float -> Float -> Coordinate.Range -> List Float
interval =
  Values.interval


-- TIME


{-| -}
time : Int -> Coordinate.Range -> List Time
time =
  Values.time
