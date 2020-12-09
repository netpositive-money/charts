module Main exposing (main)

import Http
import Svg
import Svg.Attributes
import LineChart
import LineChart.Junk as Junk
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Title as Title
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Line as AxisLine
import LineChart.Grid as Grid
import LineChart.Dots as Dots
import LineChart.Line as Line
import LineChart.Colors as Colors
import LineChart.Events as Events
import LineChart.Legends as Legends
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import Browser
import Html exposing (p, text)
import Html.Attributes exposing (style)
import Csv
import Time
import Result.Extra exposing (combineMap)
import Date exposing (fromPosix, toIsoString)

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }



-- MODEL


type alias Model =
    { csv : Maybe String
    , data : Result String Data
    , compound : Result String Data
    , factor: Float
    , offset: Float
    , hovered : Maybe Float
    , selection : Maybe Selection
    , dragging : Bool
    , hinted : Maybe Datum
    }


type alias Selection =
  { start : Float
  , end : Float
  }


type alias Data = List Datum

type alias Datum =
  {   time: Time.Posix
    , co2 : Float
  }



-- INIT


init : ( Model, Cmd Msg )
init =
  ( { csv = Nothing
    , data = Err "nothing read yet"
    , compound = Err "nothing read yet"
    , factor = 1.34 --ktCo2/day/Twh/year
    , offset = 0.1 -- MtCo2
    , hovered = Nothing
    , selection = Nothing
    , dragging = False
    , hinted = Nothing
    }
  , Http.get
      { url = "https://cbeci.org/api/csv"
      , expect = Http.expectString GotText
      }
  )




-- API


setSelection : Maybe Selection -> Model -> Model
setSelection selection model =
  { model | selection = selection }


setDragging : Bool -> Model -> Model
setDragging dragging model =
  { model | dragging = dragging }


setHovered : Maybe Float -> Model -> Model
setHovered hovered model =
  { model | hovered = hovered }


setHint : Maybe Datum -> Model -> Model
setHint hinted model =
  { model | hinted = hinted }

getSelectionStart : Float -> Model -> Float
getSelectionStart hovered model =
  case model.selection of
    Just selection -> selection.start
    Nothing        -> hovered



-- UPDATE


type Msg
  = GotText (Result Http.Error String)
  -- Chart 1
  | Hold Coordinate.Point
  | Move Coordinate.Point
  | Drop Coordinate.Point
  | LeaveChart Coordinate.Point
  | LeaveContainer Coordinate.Point
  -- Chart 2
  | Hint (Maybe Datum)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok content ->
            ( let nd = parseData content model.factor in
              { model | csv = Just content
                      , data = nd
                      , compound = mkcompound nd
              }
            , Cmd.none
            )

        Err _ ->
          (model, Cmd.none)

    Hold point ->
      model
        |> setSelection Nothing
        |> setDragging True
        |> addCmd Cmd.none

    Move point ->
      if model.dragging then
        let
          start = getSelectionStart point.x model
          newSelection = Selection start point.x
        in
        model
          |> setSelection (Just newSelection)
          |> setHovered (Just point.x)
          |> addCmd Cmd.none
      else
        model
          |> setHovered (Just point.x)
          |> addCmd Cmd.none

    Drop point ->
      if point.x == getSelectionStart point.x model then
        model
          |> setSelection Nothing
          |> setDragging False
          |> addCmd Cmd.none
      else
        model
          |> setDragging False
          |> addCmd Cmd.none

    LeaveChart point ->
      model
        |> setHovered Nothing
        |> addCmd Cmd.none

    LeaveContainer point ->
      model
        |> setDragging False
        |> setHovered Nothing
        |> addCmd Cmd.none

    Hint point ->
      model
        |> setHint point
        |> addCmd Cmd.none


mkcompound : Result String Data -> Result String Data
mkcompound nd = case nd of
                    Err x -> Err x
                    Ok data ->
                        let
                            (sum,summedList) = List.foldl f (0,[]) data
                            f d (s,l) = let ns = s+d.co2/1000 in
                                        (ns,l++[{d | co2 = ns}])
                        in Ok summedList

addCmd : Cmd Msg -> Model -> (Model, Cmd Msg)
addCmd cmd model =
    ( model, cmd )

parseData: String -> Float -> Result String Data
parseData s f = case Csv.parse s of
                  Ok c  -> combineMap (parseRecord f) c.records
                  Err _ -> Err "Toplevel parsing error"

parseRecord : Float -> List String -> Result String Datum
parseRecord factor l = case l of
                    [ts,dat,max,min,guess] -> Ok {time=Time.millisToPosix (1000 * Maybe.withDefault 0 (String.toInt ts)),
                                                  co2=factor*Maybe.withDefault 0 (String.toFloat guess)
                                                 }

                    _                      -> Err "Parsing Error"
-- VIEW


view : Model -> Html.Html Msg
view model =
  Html.div [ Html.Attributes.style "display" "flex" ] <|
    case model.csv of
    Nothing ->
      [ text "Loading ..." ]

    Just content ->
      [ p [ style "white-space" "pre" ] <|
            case model.selection of
                Nothing -> [ chart1 model
                           , chart2 model
                           ]

                Just selection ->
                           [ chart1 model
                           , chart2 model
                           , chartZoom model selection
                           ]
      ]



-- MAIN CHARTS


chart1 : Model -> Html.Html Msg
chart1 model = case model.data of
                   Ok data ->
                      LineChart.viewCustom
                          (chartConfig
                               { y=yAxis1
                               , area = Area.normal 0.5
                               , range = Range.default
                               , junk = junkConfig model
                               , legends = Legends.default
                               , events =
                                   Events.custom
                                       [ Events.onWithOptions "mousedown" (Events.Options True True False) Hold Events.getData
                                       , Events.onWithOptions "mousemove" (Events.Options True True False) Move Events.getData
                                       , Events.onWithOptions "mouseup"   (Events.Options True True True) Drop Events.getData
                                       , Events.onWithOptions "mouseleave" (Events.Options True True False) LeaveChart Events.getData
                                       , Events.onWithOptions "mouseleave" (Events.Options True True True) LeaveContainer Events.getData
                                       ]
                               , dots = Dots.custom (Dots.full 0)
                               , id = "line-chart"
                               }
                          )
                      [ LineChart.line Colors.pink Dots.circle "CO2" data ]

                   Err s ->  text s


chart2 : Model -> Html.Html Msg
chart2 model = case model.compound of
                  Ok data ->
                      LineChart.viewCustom
                          (chartConfig
                               { y = yAxis2
                               , area = Area.default
                               , range = Range.default
                               , junk = junkConfig model
                               , legends = Legends.default
                               , events =
                                   Events.custom
                                       [ Events.onWithOptions "mousedown" (Events.Options True True False) Hold Events.getData
                                       , Events.onWithOptions "mousemove" (Events.Options True True False) Move Events.getData
                                       , Events.onWithOptions "mouseup"   (Events.Options True True True) Drop Events.getData
                                       , Events.onWithOptions "mouseleave" (Events.Options True True False) LeaveChart Events.getData
                                       , Events.onWithOptions "mouseleave" (Events.Options True True True) LeaveContainer Events.getData
                                       ]
                               , dots = Dots.custom (Dots.full 0)
                               , id = "line-chart"
                               }
                          )
                          [ LineChart.line Colors.blue Dots.circle "CO2 total" data ]

                  Err s ->  text s


junkConfig : Model -> Junk.Config Datum msg
junkConfig model =
  Junk.custom <| \system ->
    { below = below system model.selection
    , above = above system model.hovered
    , html = []
    }


below : Coordinate.System -> Maybe Selection -> List (Svg.Svg msg)
below system selection =
  case selection of
    Just { start, end } ->
      [ Junk.rectangle system [ Svg.Attributes.fill "#b6b6b61a" ]
          start end system.y.min system.y.max
      ]

    Nothing ->
      []


above : Coordinate.System -> Maybe Float -> List (Svg.Svg msg)
above system maybeHovered =
  case maybeHovered of
    Just hovered ->
      [ Junk.vertical system [] hovered ]

    Nothing ->
      []



-- ZOOM CHART


chartZoom : Model -> Selection -> Html.Html Msg
chartZoom model selection = case  model.compound of
                                Ok data ->
                                    LineChart.viewCustom
                                        (chartConfig
                                             { y=yAxis1
                                             , area=Area.default
                                             , range = xAxisRangeConfig selection
                                             , junk =
                                                 Junk.hoverOne model.hinted
                                                     [ ( "date", toIsoString << fromPosix Time.utc << .time )
                                                     , ( "Mt", String.fromFloat << round100 << .co2 )
                                                     ]
                                             , events = Events.hoverOne Hint
                                             , legends = Legends.none
                                             , dots = Dots.hoverOne model.hinted
                                             , id = "line-chart-zoom"
                                             }
                                        )
                                    [ LineChart.line Colors.green Dots.circle "CO2 total" data ]
                                Err e -> text e


xAxisRangeConfig : Selection -> Range.Config
xAxisRangeConfig selection =
  let
    start =
      min selection.start selection.end

    end =
      if selection.start == selection.end
        then selection.start + 1
        else max selection.start selection.end
  in
  Range.window start end




-- VIEW CHART


type alias Config =
  { y : Axis.Config Datum Msg
  , range : Range.Config
  , junk : Junk.Config Datum Msg
  , events : Events.Config Datum Msg
  , legends : Legends.Config Datum Msg
  , dots : Dots.Config Datum
  , id : String
  , area : Area.Config
  }


chartConfig: Config -> LineChart.Config Datum Msg
chartConfig { y, range, junk, events, legends, dots, id, area } =
    { y = y
    , x = xAxis range
     , container =
        Container.custom
          { attributesHtml = [ Html.Attributes.style "font-family" "monospace" ]
          , attributesSvg = []
          , size = Container.static
          , margin = Container.Margin 30 200 60 50
          , id = id
          }
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = legends
    , events = events
    , junk = junk
    , grid = Grid.default
    , area = area
    , line = Line.default
    , dots = dots
    }

yAxis1 : Axis.Config Datum Msg
yAxis1 = Axis.default 600 "kt/day" .co2

yAxis2 : Axis.Config Datum Msg
yAxis2 = Axis.default 600 "Mt" .co2


xAxis : Range.Config -> Axis.Config Datum Msg
xAxis range = Axis.custom
          { title = Title.default "date"
          , variable = Just << datumToFloat
          , pixels = 1200
          , range = range
          , axisLine = AxisLine.rangeFrame Colors.gray
          , ticks = Ticks.time Time.utc 7
          }
-- UTILS


datumToFloat : Datum -> Float
datumToFloat = toFloat << Time.posixToMillis << .time

round100 : Float -> Float
round100 float =
  toFloat (round (float * 100)) / 100

