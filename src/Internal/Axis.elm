module Internal.Axis exposing
  ( Axis
  , int, time, float
  , intCustom, timeCustom, floatCustom, custom
  -- INTERNAL
  , ticks, viewHorizontal, viewVertical
  )


import Svg exposing (Svg, Attribute, g, text_, tspan, text)
import Svg.Attributes as Attributes exposing (class, strokeWidth, stroke)
import Lines.Axis.Tick as Tick exposing (Direction)
import Internal.Coordinate as Coordinate exposing (..)
import Internal.Axis.Tick as Tick
import Internal.Axis.Line as Line
import Internal.Axis.Intersection as Intersection
import Internal.Axis.Title as Title
import Internal.Axis.Values as Values
import Internal.Svg as Svg exposing (..)
import Internal.Utils exposing (..)



-- AXIS


{-| -}
type Axis data msg
  = Axis (Line.Line msg) (Coordinate.Range -> Coordinate.Range -> List (Tick.Tick msg))



-- API


{-| -}
int : Int -> Axis data msg
int amount =
  intCustom amount Line.default Tick.int


{-| -}
float : Int -> Axis data msg
float amount =
  floatCustom amount Line.default Tick.float


{-| -}
time : Int -> Axis data msg
time amount =
  timeCustom amount Line.default Tick.time



-- API / CUSTOM


{-| -}
intCustom : Int -> Line.Line msg -> (Int -> Tick.Tick msg) -> Axis data msg
intCustom amount line tick =
  custom line <| \data range ->
    List.map tick <| Values.int (Values.around amount) (Coordinate.smallestRange data range)


{-| -}
floatCustom : Int -> Line.Line msg -> (Float -> Tick.Tick msg) -> Axis data msg
floatCustom amount line tick =
  custom line <| \data range ->
    List.map tick <| Values.float (Values.around amount) (Coordinate.smallestRange data range)


{-| -}
timeCustom : Int -> Line.Line msg -> (Tick.Time -> Tick.Tick msg) -> Axis data msg
timeCustom amount line tick =
  custom line <| \data range ->
    List.map tick <| Values.time amount (Coordinate.smallestRange data range)



-- API / VERY CUSTOM


{-| -}
custom : Line.Line msg -> (Coordinate.Range -> Coordinate.Range -> List (Tick.Tick msg)) -> Axis data msg
custom =
  Axis



-- INTERNAL


ticks : Coordinate.Range -> Coordinate.Range -> Axis data msg -> List (Tick.Tick msg)
ticks dataRange range (Axis line values) =
  values dataRange range


line : Axis data msg -> Coordinate.Range -> Coordinate.Range -> Line.Config msg
line (Axis line _) =
  Line.config line



-- INTERNAL / VIEW


type alias ViewConfig msg =
  { line : Coordinate.Range -> Coordinate.Range -> Line.Config msg
  , ticks : List (Tick.Tick msg)
  , intersection : Float
  , title : Title.Config msg
  }


{-| -}
viewHorizontal : Coordinate.System -> Intersection.Intersection -> Title.Title msg -> Axis data msg -> Svg msg
viewHorizontal system intersection title axis =
    let
        config =
          { line = line axis
          , ticks = ticks system.xData system.x axis
          , intersection = Intersection.getY intersection system
          , title = Title.config title
          }

        at x =
          { x = x, y = config.intersection }

        viewAxisLine =
          viewHorizontalAxisLine system config.intersection

        viewTick tick =
          viewHorizontalTick system config (at tick.position) tick
    in
    g [ class "chart__axis--horizontal" ]
      [ viewHorizontalTitle system at config
      , viewAxisLine (config.line system.xData system.x)
      , g [ class "chart__ticks" ] (List.map viewTick config.ticks)
      ]


{-| -}
viewVertical : Coordinate.System -> Intersection.Intersection -> Title.Title msg -> Axis data msg -> Svg msg
viewVertical system intersection title axis =
    let
        config =
          { line = line axis
          , ticks = ticks system.yData system.y axis
          , intersection = Intersection.getX intersection system
          , title = Title.config title
          }

        at y =
          { x = config.intersection, y = y }

        viewAxisLine =
          viewVerticalAxisLine system config.intersection

        viewTick tick =
          viewVerticalTick system config (at tick.position) tick
    in
    g [ class "chart__axis--vertical" ]
      [ viewVerticalTitle system at config
      , viewAxisLine (config.line system.yData system.y)
      , g [ class "chart__ticks" ] (List.map viewTick config.ticks)
      ]



-- INTERNAL / VIEW / TITLE


viewHorizontalTitle : Coordinate.System -> (Float -> Point) -> ViewConfig msg -> Svg msg
viewHorizontalTitle system at { title } =
  let
    position =
      at (title.position system.x)
  in
  g [ class "chart__title"
    , transform
        [ move system position.x position.y
        , offset title.xOffset (title.yOffset + 40)
        ]
    , anchorStyle Middle
    ]
    [ title.view ]


viewVerticalTitle : Coordinate.System -> (Float -> Point) -> ViewConfig msg -> Svg msg
viewVerticalTitle system at { title } =
  let
    position =
      at (title.position system.y)
  in
  g [ class "chart__title"
    , transform
        [ move system position.x position.y
        , offset (title.xOffset - 5) (title.yOffset - 15)
        ]
    , anchorStyle Middle
    ]
    [ title.view ]



-- INTERNAL / VIEW / LINE


viewHorizontalAxisLine : Coordinate.System -> Float -> Line.Config msg -> Svg msg
viewHorizontalAxisLine system axisPosition config =
  horizontal system (attributesLine config) axisPosition config.start config.end


viewVerticalAxisLine : Coordinate.System -> Float -> Line.Config msg -> Svg msg
viewVerticalAxisLine system axisPosition config =
  vertical system (attributesLine config) axisPosition config.start config.end


attributesLine : Line.Config msg -> List (Svg.Attribute msg)
attributesLine { events, width, color } =
  events ++ [ strokeWidth (toString width), stroke color ]



-- INTERNAL / VIEW / TICK


viewHorizontalTick : Coordinate.System -> ViewConfig msg -> Point -> Tick.Tick msg -> Svg msg
viewHorizontalTick system config ({ x, y } as point) tick =
  g [ class "chart__tick" ]
    [ xTick system (lengthOfTick tick) (attributesTick tick) y x
    , viewMaybe tick.label (viewHorizontalLabel system tick point)
    ]


viewVerticalTick : Coordinate.System -> ViewConfig msg -> Point -> Tick.Tick msg -> Svg msg
viewVerticalTick system config ({ x, y } as point) tick =
  g [ class "chart__tick" ]
    [ yTick system (lengthOfTick tick) (attributesTick tick) x y
    , viewMaybe tick.label (viewVerticalLabel system tick point)
    ]


lengthOfTick : Tick.Tick msg -> Float
lengthOfTick { length, direction } =
  if Tick.isPositive direction then -length else length


attributesTick : Tick.Tick msg -> List (Svg.Attribute msg)
attributesTick { width, color } =
  [ strokeWidth (toString width), stroke color ]


viewHorizontalLabel : Coordinate.System -> Tick.Tick msg -> Point -> Svg msg -> Svg msg
viewHorizontalLabel system { direction, length } position view =
  let
    yOffset = if Tick.isPositive direction then -5 - length else 15 + length
  in
  g [ transform [ move system position.x position.y, offset 0 yOffset ]
    , anchorStyle Middle
    ]
    [ view ]


viewVerticalLabel : Coordinate.System -> Tick.Tick msg -> Point -> Svg msg -> Svg msg
viewVerticalLabel system { direction, length } position view =
  let
    anchor = if Tick.isPositive direction then Start else End
    xOffset = if Tick.isPositive direction then 5 + length else -5 - length
  in
  g [ transform [ move system position.x position.y, offset xOffset 5 ]
    , anchorStyle anchor
    ]
    [ view ]
