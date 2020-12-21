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
import Html exposing (p, text, input)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onInput)
import Csv
import Time exposing (Posix,utc,millisToPosix,posixToMillis)
import Iso8601 exposing (fromTime, toTime)
import List.Extra exposing (find)

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
    , hovered : Maybe Datum
    , selection : Selection
    , dragging : Bool
    , hinted : Maybe Datum
    , startString : String
    , endString: String
    , btcS: String
    }


type alias Selection =
  { start : Maybe Datum
  , end : Maybe Datum
  }


type alias Data = List Datum

type alias Datum =
  {   time: Posix
    , co2 : Float
  }



-- INIT

emptySelection: Selection
emptySelection = {start=Nothing,end=Nothing}

init : ( Model, Cmd Msg )
init =
  ( { csv = Nothing
    , data = Err "nothing read yet"
    , compound = Err "nothing read yet"
    , factor = 1.34 --ktCo2/day/Twh/year
    , offset = 0.049 -- MtCo2
    , hovered = Nothing
    , selection = emptySelection
    , dragging = False
    , hinted = Nothing
    , startString="YYYY-MM-DD"
    , endString="YYYY-MM-DD"
    , btcS=""
    }
  , Http.get
      { url = "https://cbeci.org/api/csv"
      , expect = Http.expectString GotText
      }
  )




-- API


setSelection : Selection -> Model -> Model
setSelection selection model =
  { model | selection = selection }


setDragging : Bool -> Model -> Model
setDragging dragging model =
  { model | dragging = dragging }


setHovered : Maybe Datum -> Model -> Model
setHovered hovered model =
  { model | hovered = hovered }


setHint : Maybe Datum -> Model -> Model
setHint hinted model =
  { model | hinted = hinted }

getSelectionStart : Datum -> Model -> Datum
getSelectionStart hovered model =
  case model.selection.start of
    Just s ->  s
    Nothing -> hovered

setSelectionString : Datum -> Datum -> Model -> Model
setSelectionString start end model =
  { model | startString = start |> datumToTimeString
          , endString   = end |> datumToTimeString
  }


-- UPDATE


type Msg
  = GotText (Result Http.Error String)
  -- Chart 1
  | Hold (Maybe Datum)
  | Move (Maybe Datum)
  | Drop (Maybe Datum)
  | LeaveChart (Maybe Datum)
  | LeaveContainer (Maybe Datum)
  -- Chart 2
  | Hint (Maybe Datum)
  | ChangeStart String
  | ChangeEnd String
  | ChangeBtc String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok content ->
            ( let nd = parseData content model.factor
                  cp = mkcompound nd
                  s = Result.withDefault Nothing <| Result.map List.head cp
                  e = Result.withDefault Nothing <| Result.map
                      (\l -> List.head <| List.drop (List.length l - 1) l) cp
                  dTS = Maybe.withDefault "YYYY-MM-DD" << Maybe.map datumToTimeString
              in
              { model | csv = Just content
                      , data = nd
                      , compound = cp
                      , selection = { start = s
                                    , end = e
                                    }
                      , startString = dTS s
                      , endString = dTS e
              }
            , Cmd.none
            )

        Err _ ->
          (model, Cmd.none)

    Hold point ->
      model
        |> setSelection emptySelection
        |> setDragging True
        |> addCmd Cmd.none

    Move (Just point) ->
      if model.dragging then
        let
          start = getSelectionStart point model
          newSelection = Selection (Just start) (Just point)
        in
        model
          |> setSelection newSelection
          |> setSelectionString start point
          |> setHovered (Just point)
          |> addCmd Cmd.none
      else
        model
          |> setHovered (Just point)
          |> addCmd Cmd.none

    Drop (Just point) ->
        if point == getSelectionStart point model then
        model
          |> setSelection emptySelection
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

    ChangeStart timeString -> addCmd Cmd.none <| case model.compound of
        Err _ -> model
        Ok ls -> let selection=model.selection in
            { model | startString = timeString
                    , selection= case toTime timeString of
                        Ok ts -> {selection | start=find (\d->d.time==ts) ls}
                        Err _ -> selection
            }

    ChangeEnd timeString -> addCmd Cmd.none <| case model.compound of
        Err _ -> model
        Ok ls -> let selection=model.selection in
            { model | endString = timeString
                    , selection= case toTime timeString of
                Ok ts -> {selection | end=find (\d->d.time==ts) ls}
                Err _ -> selection
            }

    ChangeBtc bS -> addCmd Cmd.none <|
       {  model | btcS = bS }

    _  ->
        model  |> addCmd Cmd.none


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
                  Ok c  -> Ok (List.filterMap (parseRecord f) c.records)
                  Err _ -> Err "Toplevel parsing error"

parseRecord : Float -> List String -> Maybe Datum
parseRecord factor l = case l of
                    [ts,dat,max,min,guess] -> let time=millisToPosix (1000 * Maybe.withDefault 0 (String.toInt ts))
                                                  co2=factor*Maybe.withDefault 0 (String.toFloat guess)
                                              in
                                                 -- if Time.toWeekday Time.utc time == Time.Mon then
                                                      Just { time=time, co2=co2 }
                                                 -- else Nothing

                    _                      -> Nothing
-- VIEW


view : Model -> Html.Html Msg
view model =
  Html.div [ Html.Attributes.style "display" "flex" ] <|
    case model.csv of
    Nothing ->
      [ text "Loading ..." ]

    Just content ->
      [ p [ style "white-space" "pre" ] <|
            let s=model.startString
                e=model.endString
            in case (model.selection.start, model.selection.end) of
                (Just startDatum, Just endDatum) ->
                    let total = endDatum.co2-startDatum.co2 in
                    [ chart1 model
                    , chart2 model
                    , text ("The horizontal line at "++String.fromFloat model.offset++" Mt signifies the amount of Co2 that has already been offset today.")
                    , text ("\nCalculated from the Genesis block at January 3, 2009, this means we've offset Bitcoin's history approximately until "++findOffsetDate model++".")
                    , text ("\nPlease select or enter time interval: ")
                    , input [ placeholder s, value s, onInput ChangeStart][]
                    , input [ placeholder e, value e, onInput ChangeEnd][]
                 --   , chartZoom model startDatum endDatum
                    , text ("\nSelected: "++ datumToTimeString startDatum)
                    , text (" to "++ datumToTimeString endDatum)
                    , text ("\nTotal Co2 in this time frame: "
                                ++ (String.fromFloat <| round100 <| total)
                                ++ "Mt"
                           )
                    , text "\nHow much Bitcoin do you want to offset? "
                    , input [ placeholder "0.00000001", value model.btcS, onInput ChangeBtc][]
                    ] ++
                    case String.toFloat model.btcS of
                      Nothing -> []
                      Just btc -> [ text ("\nThis is equivalent to "++(String.fromFloat <| round100 <| total*btc/21)
                                       ++" t Co2.\nHappy offsetting, and don't forget to tell us about it so we can keep count!") ]


                _       -> [ chart1 model
                           , chart2 model
                           , text "Please select or enter a time interval: "
                           , input [ placeholder "start date", value s, onInput ChangeStart][]
                           , input [ placeholder "end date", value e, onInput ChangeEnd][]
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
                               , junk =
                                   Junk.hoverOne model.hinted
                                       [ ( "date", datumToTimeString )
                                       , ( "kt/d", String.fromFloat << round100 << .co2 )
                                       ]
                               , events = Events.hoverOne Hint
                               , legends = Legends.default
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
                                       [ Events.onWithOptions "mousedown" (Events.Options True True False) Hold Events.getNearest
                                       , Events.onWithOptions "mousemove" (Events.Options True True False) Move Events.getNearest
                                       , Events.onWithOptions "mouseup"   (Events.Options True True True) Drop Events.getNearest
                                       , Events.onWithOptions "mouseleave" (Events.Options True True False) LeaveChart Events.getNearest
                                       , Events.onWithOptions "mouseleave" (Events.Options True True True) LeaveContainer Events.getNearest
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
    , above = above system model.hovered model.offset
    , html = []
    }


below : Coordinate.System -> Selection -> List (Svg.Svg msg)
below system selection =
  case (selection.start, selection.end) of
    ( Just startDatum, Just endDatum ) ->
      [ Junk.rectangle system [ Svg.Attributes.fill "#b6b6b61a" ]
          (datumToFloat startDatum) (datumToFloat endDatum) system.y.min system.y.max
      ]

    _ ->
      []


above : Coordinate.System -> Maybe Datum -> Float -> List (Svg.Svg msg)
above system maybeHovered offset =
    Junk.horizontal system [] offset ::
  case maybeHovered of
    Just hovered ->
      [ Junk.vertical system [] (datumToFloat hovered) ]

    Nothing ->
      []


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
yAxis1 = Axis.full 600 "kt/day" .co2

yAxis2 : Axis.Config Datum Msg
yAxis2 = Axis.full 600 "Mt" .co2


xAxis : Range.Config -> Axis.Config Datum Msg
xAxis range = Axis.custom
          { title = Title.default "date"
          , variable = Just << datumToFloat
          , pixels = 1200
          , range = range
          , axisLine = AxisLine.full Colors.gray
          , ticks = Ticks.time utc 7
          }

-- UTILS


datumToFloat : Datum -> Float
datumToFloat = toFloat << posixToMillis << .time

datumToTimeString : Datum -> String
datumToTimeString = String.left 10 << fromTime << .time

round100 : Float -> Float
round100 float =
  toFloat (round (float * 100)) / 100

findOffsetDate : Model -> String
findOffsetDate model = let fOD s l = case l of
                                         [] -> s
                                         x::xs -> if x.co2 < model.offset
                                                 then fOD (datumToTimeString x) xs
                                                 else s
                       in
                           case model.data of
                               Err _ -> "(data error)"
                               Ok d  -> fOD "2009-03-01" d
