module Main exposing (..)

import Basics exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task
import Mouse exposing (..)


dialRadius = 100.0
outerRadius = 80.0
innerRadius = 54
tickRadius = 13.0


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { state : State
  , hour : Int
  , minute : Int
  }


type State
  = HourView
  | MinuteView
  | Closed


init : (Model, Cmd Msg)
init =
  (Model Closed 0 0, Cmd.none)


type Msg
  = NoOp
  | OpenPicker
  | ClosePicker
  | SetHour Int
  | SetMinute Int
  | DragAt Position
  | DragEnd Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    OpenPicker ->
      ({model | state = HourView}, Cmd.none)

    ClosePicker ->
      ({model | state = Closed}, Cmd.none)

    SetHour hour ->
      ({model | hour = hour, state = MinuteView}, Cmd.none)

    SetMinute minute ->
      ({model | minute = minute, state = Closed}, Cmd.none)

    DragAt position ->
      (model, Cmd.none)

    DragEnd position ->
      (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Closed ->
      Sub.none

    HourView ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

    MinuteView ->
      Sub.none


view : Model -> Html Msg
view model =
  div
    [ style [ ("background-color", "green"), ("position", "relative") ]
    , onBlur ClosePicker
    ]
    [ p [] [ text "Clockpicker" ]
    , input
      [ onClick OpenPicker
      , value (formatTime model)
      ]
      []
    , clockPickerWrapper model
    ]


formatTime : Model -> String
formatTime model =
  (formatHourFull model.hour) ++ ":" ++ (formatMinuteFull model.minute)


clockPickerWrapper : Model -> Html Msg
clockPickerWrapper model =
  case model.state of
    Closed ->
      text ""

    HourView ->
      drawHourView model

    MinuteView ->
      drawMinuteView model


drawHourView : Model -> Html Msg
drawHourView model =
  div
    []
    [ drawHourTicks model ]


drawHourTicks : Model -> Html Msg
drawHourTicks model =
  div
    []
    (List.map drawHourTick (List.range 1 24))


drawHourTick : Int -> Html Msg
drawHourTick tick =
  let
    radius = if tick > 12 then innerRadius else outerRadius
    radian = (toFloat tick) / 6 * pi
    left = dialRadius + (sin radian) * radius - tickRadius
    top = dialRadius - (cos radian) * radius - tickRadius
  in
    div
      [ style
          [ ("left", (toString left) ++ "px")
          , ("top", (toString top) ++ "px")
          , ("position", "absolute")
          , ("textAlign", "center")
          , ("width", "26px")
          , ("height", "26px")
          , ("user-select", "none")
          , ("cursor", "pointer")
          ]
      , onClick (SetHour tick)
      ]
      [ text (formatHour tick)  ]


formatHour : Int -> String
formatHour hour =
  case hour of
    24 -> "00"
    _ -> toString hour


formatHourFull : Int -> String
formatHourFull hour =
  if hour == 24 then
    "00"
  else if hour < 10 then
    "0" ++ (toString hour)
  else
    (toString hour)


drawMinuteView : Model -> Html Msg
drawMinuteView model =
  div
    []
    [ drawMinuteTicks model ]


drawMinuteTicks : Model -> Html Msg
drawMinuteTicks model =
  div
    []
    (List.map drawMinuteTick (List.range 1 (60 // 5)))


drawMinuteTick : Int -> Html Msg
drawMinuteTick tick =
  let
    minute = tick * 5
    radius = outerRadius
    radian = (toFloat tick) / 6 * pi
    left = dialRadius + (sin radian) * radius - tickRadius
    top = dialRadius - (cos radian) * radius - tickRadius
  in
    div
      [ style
        [ ("left", (toString left) ++ "px")
        , ("top", (toString top) ++ "px")
        , ("position", "absolute")
        , ("textAlign", "center")
        , ("width", "26px")
        , ("height", "26px")
        , ("cursor", "pointer")
        ]
      , onClick (SetMinute minute)
      ]
      [ text (formatMinute minute) ]


formatMinute : Int -> String
formatMinute minute =
  case minute of
    60 -> "00"
    _ -> toString minute


formatMinuteFull : Int -> String
formatMinuteFull minute =
  if minute == 60 then
    "00"
  else if minute < 10 then
    "0" ++ (toString minute)
  else
    toString minute



