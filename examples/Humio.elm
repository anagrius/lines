module Humio exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Lines as Lines
import Lines.Area as Area
import Lines.Axis as Axis
import Lines.Axis.Intersection as Intersection
import Lines.Axis.Range as Range
import Lines.Axis.Title as Title
import Lines.Color as Color
import Lines.Coordinate as Coordinate
import Lines.Dimension as Dimension
import Lines.Dot as Dot
import Lines.Events as Events
import Lines.Grid as Grid
import Lines.Junk as Junk exposing (..)
import Lines.Legends as Legends
import Lines.Line as Line
import Svg exposing (Attribute, Svg, g, text, text_)
import Svg.Attributes as Attributes


type alias Point =
    ( Float, Maybe Float )


color1 : String
color1 =
    "#6c51af"


color2 : String
color2 =
    "#e2921a"


color3 : String
color3 =
    "#26beee"


nearestColor : String
nearestColor =
    "#000000"


type Msg
    = NoOp
    | Hover String ( Coordinate.Point, Maybe ( Float, Maybe Float ) )
    | StartDragging Coordinate.Point
    | EndSelection
    | ClearHover


type alias SelectionState =
    { startX : Float, endX : Float }


type alias Model =
    { hoveredPoint : Maybe Float
    , line : Maybe Float
    , selectedTimeRange : Maybe SelectionState
    , selectionState : Maybe SelectionState
    , hoveredId : Maybe String
    }


init : Model
init =
    { hoveredPoint = Nothing
    , line = Nothing
    , selectedTimeRange = Nothing
    , selectionState = Nothing
    , hoveredId = Nothing
    }


flatmap : (x -> Maybe y) -> Maybe x -> Maybe y
flatmap f x =
    case x of
        Just x ->
            f x

        Nothing ->
            Nothing


when : Bool -> (() -> Maybe a) -> Maybe a
when condition f =
    if condition then
        f ()
    else
        Nothing


contains : a -> Maybe a -> Bool
contains v maybe =
    maybe == Just v


whenEmpty : Maybe a -> (() -> Maybe b) -> Maybe b
whenEmpty maybe f =
    when (maybe == Nothing) f


indexOf : x -> List x -> Maybe Int
indexOf x xs =
    let
        find i vals =
            case vals of
                [] ->
                    Nothing

                y :: rest ->
                    if y == x then
                        Just i
                    else
                        find (i + 1) rest
    in
    find 0 xs


type alias Legend =
    List
        { title : String
        , value : Maybe Float
        , color : String
        }


theJunk : Model -> Chart -> String -> List Series -> Junk.Junk msg
theJunk { hoveredId, selectionState, line, hoveredPoint } { bucketSize, xs } currentId series =
    let
        legend : Maybe Legend
        legend =
            case flatmap (\x -> indexOf x xs) hoveredPoint of
                Just i ->
                    Just <|
                        List.map
                            (\s ->
                                { title = s.title
                                , value = s.ys |> List.drop i |> List.head |> flatten
                                , color = s.color
                                }
                            )
                            series

                Nothing ->
                    Nothing
    in
    Junk.custom <|
        \system ->
            { below = []
            , above =
                List.filterMap identity
                    [ Maybe.map (selectionArea system (contains currentId hoveredId)) selectionState
                    , whenEmpty selectionState <| \_ -> Maybe.map (hoverLine system 0.1) line
                    , when (contains currentId hoveredId && selectionState == Nothing) <|
                        \_ -> Maybe.map (bucketBand system 0.05 bucketSize) hoveredPoint
                    ]
            , html =
                List.filterMap identity
                    [ when (contains currentId hoveredId && selectionState == Nothing) <|
                        \_ -> Maybe.map2 (tooltip system bucketSize) legend hoveredPoint
                    ]
            }


px : a -> String
px v =
    toString v ++ "px"


tooltip : Coordinate.System -> Int -> Legend -> Float -> Html msg
tooltip system bucketSize legend x =
    let
        w =
            200

        itemHeight =
            14

        count =
            List.length legend

        padding =
            8

        hGap =
            6

        h =
            toFloat <| count * itemHeight + (count - 1) * hGap + 2 * padding

        gap =
            10 + Coordinate.scaleSVGX system (toFloat bucketSize)

        viewItem { title, value, color } =
            [ Html.span
                [ class "chart__tooltip-item-title"
                , Html.Attributes.style
                    [ ( "height", px itemHeight )
                    , ( "color", color )
                    ]
                ]
                [ Html.text title ]
            , Html.span [ class "chart__tooltip-item-value" ] [ Html.text (Maybe.withDefault "N/A" (Maybe.map toString value)) ]
            ]
    in
    div
        [ class "chart__tooltip"
        , Html.Attributes.style
            [ ( "height", px h )
            , ( "width", px w )
            , ( "background", "rgba(255,255,255,0.8)" )
            , ( "left", px <| Coordinate.toSVGX system x + gap )
            , ( "top", px <| (system.frame.size.height - system.frame.margin.top - system.frame.margin.bottom) / 2 - h / 2 + system.frame.margin.top )
            ]
        ]
        (List.concatMap viewItem legend)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Hover id ( line, nearestPointBefore ) ->
            { model
                | line = Just line.x
                , hoveredPoint = Maybe.map Tuple.first nearestPointBefore
                , selectionState = Maybe.map (\s -> { s | endX = line.x }) model.selectionState
                , hoveredId = Just id
            }

        ClearHover ->
            { model
                | line = Nothing
                , selectionState = Nothing
                , hoveredId = Nothing
                , hoveredPoint = Nothing
            }

        StartDragging point ->
            { model | selectionState = Just { startX = point.x, endX = point.x } }

        EndSelection ->
            case model.selectionState of
                Just state ->
                    { model | selectedTimeRange = Just state, selectionState = Nothing }

                Nothing ->
                    model


selectionArea : Coordinate.System -> Bool -> SelectionState -> Svg msg
selectionArea system isPrimary { startX, endX } =
    let
        lineColor =
            if isPrimary then
                "rgb(124, 0, 233)"
            else
                "rgba(0, 0, 0, 0.3)"

        color =
            if isPrimary then
                "rgba(124, 0, 233, 0.1)"
            else
                "rgba(0, 0, 0, 0.1)"

        min =
            Coordinate.scaleDataY system system.y.min

        max =
            system.y.max

        bottomLineWidth =
            Coordinate.scaleDataY system 3
    in
    Svg.g []
        [ Junk.rectangle system [ Attributes.fill color ] startX endX min max
        , Junk.vertical system [ Attributes.stroke lineColor, Attributes.strokeDasharray "3,3" ] startX min max
        , Junk.vertical system [ Attributes.stroke lineColor, Attributes.strokeDasharray "3,3" ] endX min max

        --, Junk.rectangle system [ Attributes.fill lineColor ] startX endX (min - bottomLineWidth) min
        ]


hoverLine : Coordinate.System -> Float -> Float -> Svg msg
hoverLine system opacity date =
    Junk.vertical system
        [ Attributes.strokeWidth "1px"
        , Attributes.stroke "purple"
        , Attributes.opacity "0.5"
        ]
        date
        (Coordinate.scaleDataY system system.y.min)
        system.y.max


bucketBand : Coordinate.System -> Float -> Int -> Float -> Svg msg
bucketBand system opacity bucketSize date =
    Junk.vertical system
        [ Attributes.strokeWidth <| px (Coordinate.scaleSVGX system (toFloat bucketSize))
        , Attributes.stroke "black"
        , Attributes.opacity (toString opacity)
        ]
        (date + (toFloat bucketSize / 2))
        (Coordinate.scaleDataY system system.y.min)
        system.y.max


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, update = update, view = view }


cellSize : Int
cellSize =
    100


gapSize =
    16


headerHeight =
    22


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style"
            []
            [ Html.text <| """
.dashboard__widgets {
  display: grid;
  grid-template-columns: repeat(12, """ ++ toString cellSize ++ """px);
  grid-auto-rows: """ ++ toString cellSize ++ """px;
  grid-gap: """ ++ toString gapSize ++ """px;
  width: 100%;
  padding: 16px;
}

.dashboard__widget {
  font-family: monospace;
  font-size: 10px;
  border: 1px solid #efefef;
  padding: 8px;
  box-sizing: border-box;
}

.dashboard__widget-title {
  text-align: center;
  font-size: 12px;
  padding-bottom: 8px;
  font-weight: bold;
}

.dashboard__iframe {
  border: none;
  height: 100%;
  width: 100%;
}

.chart__tooltip {
  position: absolute;
  z-index: 2;
  pointer-events: none;
  border: 1px solid #CFCFCF;
  border-radius: 4px;
  padding: 8px 16px;
  box-sizing: border-box;
  display: grid;
  grid-gap: 6px;
  grid-template-columns: auto auto;
}

.chart__tooltip-item {
  display: contents;
}
""" ]
        , div
            [ class "dashboard__widgets" ]
            [ viewChart model chart1
            , viewChart model chart2

            -- , viewChart [] model "2"
            -- , viewChart [] model "3"
            , viewIframe "https://www.humio.com" model "4"
            ]
        ]


flatten : Maybe (Maybe a) -> Maybe a
flatten maybe =
    Maybe.withDefault Nothing maybe


now : number
now =
    1515878745894


widgetPadding : number
widgetPadding =
    8


chart1 : Chart
chart1 =
    let
        bucketSize =
            3600000
    in
    { xLabel = Just "Time"
    , yLabel = Just "Eggs"
    , bucketSize = bucketSize
    , series =
        [ { title = "Agda", color = color1, ys = List.map Just [ 0, 1, 3, 5, 7, 1, 2, 1, 2, 2 ] }
        , { title = "Marie", color = color2, ys = List.map Just [ 1, 0, 0, 2, 1, 1, 2, 1, 2, 2 ] }
        , { title = "Sofie", color = color3, ys = List.map Just [ 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ] }
        ]
    , xs = List.range 0 9 |> List.map (\i -> toFloat <| now + bucketSize * i)
    , title = "Chickens"
    , id = "chickens"
    }


chart2 : Chart
chart2 =
    let
        bucketSize =
            3600000 * 24
    in
    { xLabel = Just "Time"
    , yLabel = Just "Mean"
    , bucketSize = bucketSize
    , series =
        [ { title = "@therealdonaldtrump", color = color1, ys = [ Just 1, Just 21, Just 211, Just 121, Just 0, Just 1, Just 1, Just 101, Just 171, Just 171 ] }
        , { title = "@anagrius", color = color2, ys = [ Just 0, Just 10, Just 0, Just 10, Just 120, Just 5, Just 5, Just 0, Just 70, Just 70 ] }
        , { title = "@meethumio", color = color3, ys = [ Just 19, Just 0, Just 0, Just 100, Just 0, Just 0, Just 2, Just 16, Just 2, Just 2 ] }
        ]
    , xs = List.range 0 9 |> List.map (\i -> toFloat <| now - bucketSize * 3 + bucketSize * i)
    , title = "Tweets"
    , id = "tweets"
    }


type alias Series =
    { title : String
    , ys : List (Maybe Float)
    , color : String
    }


type alias Chart =
    { xLabel : Maybe String
    , yLabel : Maybe String
    , series : List Series
    , xs : List Float
    , title : String
    , bucketSize : Int
    , id : String
    }


type alias Frame =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


color4 : String
color4 =
    "#2c955c"


color5 : String
color5 =
    "#cc7b91"


colors : List String
colors =
    [ color1
    , color2
    , color3
    , color4
    , color5
    ]


colorAtIndex : Int -> String
colorAtIndex i =
    colors |> List.drop i |> List.head |> Maybe.withDefault "#000000"


viewChart :
    Model
    -> Chart
    -> Html Msg
viewChart model chart =
    let
        lines =
            chart.series
                |> List.indexedMap
                    (\i series ->
                        let
                            data =
                                List.map2 (,) chart.xs series.ys
                        in
                        Lines.line (colorAtIndex i) Dot.none series.title data
                    )
    in
    div
        [ Attributes.class "dashboard__widget"
        , Attributes.style "grid-column-start: span 6; grid-row-start: span 3;"
        , HtmlEvents.onWithOptions "mousedown" { preventDefault = True, stopPropagation = False } (Decode.succeed NoOp)
        , Html.Attributes.style [ ( "position", "relative" ) ]
        ]
        [ Html.div [ class "dashboard__widget-title" ] [ Html.text chart.title ]
        , Lines.viewCustom
            { margin = Coordinate.Margin 10 120 30 30
            , attributes = []
            , grid = Grid.lines 1 Color.grayLight
            , area = Area.stacked 0.2
            , events =
                Events.custom
                    [ Events.onMouseDown StartDragging Events.getData
                    , Events.onMouseUp (\_ -> EndSelection) Events.getData
                    , Events.onMouseLeave ClearHover
                    , Events.onMouseMove (Hover chart.id) <|
                        Events.map2 (,) Events.getData Events.getNearestXBefore
                    ]
            , x =
                { title = Title.default (Maybe.withDefault "" chart.xLabel)
                , variable = Tuple.first >> Just
                , pixels = cellSize * 6 + gapSize * (6 - 1) - widgetPadding * 2
                , range =
                    Range.custom
                        (\dataRange ->
                            ( dataRange.min
                            , dataRange.max
                              --+ toFloat chart.bucketSize
                            )
                        )
                , axis = Axis.time 10
                }
            , y =
                { title = Title.default (Maybe.withDefault "" chart.yLabel)
                , variable = Tuple.second
                , pixels = cellSize * 3 + gapSize * (3 - 1) - headerHeight - widgetPadding * 2
                , range = Range.default
                , axis = Axis.time 10
                }
            , intersection = Intersection.default
            , junk = theJunk model chart chart.id chart.series
            , interpolation = Lines.steppedAfter
            , legends = Legends.default
            , line = Line.default
            , dot = Dot.default
            , id = chart.id
            }
            lines
        ]


getColor : String -> List data -> Maybe data -> String
getColor color series maybeX =
    case maybeX of
        Just nearest ->
            if List.member nearest series then
                nearestColor
            else
                color

        Nothing ->
            color



-- pie : Svg Msg
-- pie =
--     Pies.view
--         { x = 1
--         , y = EndSelection
--         , width = 500
--         , height = 500
--         }
--         [ Pies.wedge Color.blue "Rain" 102
--         , Pies.wedge Color.orange "Sun" 12
--         , Pies.wedge Color.pink "Cloudy" 42
--         ]
-- tick : Int -> (x,y) -> Tick.Tick msg
-- tick _ data =
--     { color = Color.orange
--     , width = 2
--     , length = 7
--     , grid = False
--     , label = Just <| Junk.text Color.blue (toString data.heartattacks)
--     , direction = Tick.negative
--     , position = 0
--     }
--
-- DATA
--
--
-- type alias Data =
--     { magnesium : Float
--     , heartattacks : Float
--     , date : Maybe Float
--     }
--
--
-- data1 : List Data
-- data1 =
--     [ Data 1 1 (Just <| 269810504300 + (1 + 0) * 3600000)
--     , Data 2 2 (Just <| 269810504300 + (1 + 1) * 3600000)
--     , Data 3 4 (Just <| 269810504300 + (1 + 2) * 3600000)
--     , Data 9 2 Nothing
--     , Data 8 5 (Just <| 269810504300 + (1 + 4) * 3600000)
--     , Data 8 1 (Just <| 269810504300 + (1 + 5) * 3600000)
--     , Data 2 3 Nothing
--     , Data 3 3 (Just <| 269810504300 + (1 + 7) * 3600000)
--     , Data 9 8 (Just <| 269810504300 + (1 + 8) * 3600000)
--     ]
--
--
-- data2 : List Data
-- data2 =
--     [ Data 2 1 (Just <| 269810504300 + (1 + 0) * 3600000)
--     , Data 3 2 (Just <| 269810504300 + (1 + 1) * 3600000)
--     , Data 4 3 (Just <| 269810504300 + (1 + 2) * 3600000)
--     , Data 5 2 Nothing
--     , Data 4 5 (Just <| 269810504300 + (1 + 4) * 3600000)
--     , Data 6 3 (Just <| 269810504300 + (1 + 5) * 3600000)
--     , Data 4 5 Nothing
--     , Data 9 8 (Just <| 269810504300 + (1 + 7) * 3600000)
--     , Data 4 3 (Just <| 269810504300 + (1 + 8) * 3600000)
--     ]
--
--
-- data3_a : List Data
-- data3_a =
--     [ Data 2 1 (Just <| 269810504300 + (1 + 0) * 3600000)
--     , Data 3 2 (Just <| 269810504300 + (1 + 1) * 3600000)
--     , Data 4 2 (Just <| 269810504300 + (1 + 2) * 3600000)
--     , Data 5 1 Nothing
--     , Data 8 4 (Just <| 269810504300 + (1 + 4) * 3600000)
--     , Data 8 6 (Just <| 269810504300 + (1 + 5) * 3600000)
--     , Data 2 9 Nothing
--     , Data 3 7 (Just <| 269810504300 + (1 + 7) * 3600000)
--     , Data 9 3 (Just <| 269810504300 + (1 + 8) * 3600000)
--     ]
--


viewIframe :
    String
    -> Model
    -> String
    -> Html Msg
viewIframe url model id =
    div
        [ Attributes.class "dashboard__widget"
        , Attributes.style "grid-column-start: span 6; grid-row-start: span 4;"
        , HtmlEvents.onWithOptions "mousedown" { preventDefault = True, stopPropagation = False } (Decode.succeed NoOp)
        , Html.Attributes.style [ ( "position", "relative" ) ]
        ]
        [ Html.iframe [ Attributes.class "dashboard__iframe", Html.Attributes.src url ] []
        ]
