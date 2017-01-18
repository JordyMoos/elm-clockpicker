module Main exposing (..)

import Basics exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (..)
import Json.Decode as Json
import Svg
import Svg.Attributes
import Svg.Events
import VirtualDom

import Model exposing (..)
import Msg exposing (Msg(..))
import Hour exposing (..)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
  (Model Closed 0 0 <| Position 0 0, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    OpenPicker ->
      ({model | state = HourView}, Cmd.none)

    ClosePicker ->
      ({model | state = Closed}, Cmd.none)

    GuessHour ->
      let
        x = (toFloat model.pos.x) - dialRadius
        y = (toFloat model.pos.y) - dialRadius

        radianTemp = atan2 x (negate y)
        radian = if radianTemp < 0 then pi * 2 + radianTemp else radianTemp

        z = sqrt <| x * x + y * y
        isInner = if z < ((outerRadius + innerRadius) / 2) then True else False

        unit = 1 / 6 * pi
        val = round <| radian / unit
        hour = valToHour val isInner
      in
        {model | hour = hour, state = MinuteView} ! []

    SetHour hour ->
      ({model | hour = hour, state = MinuteView}, Cmd.none)

    SetMinute minute ->
      ({model | minute = minute, state = Closed}, Cmd.none)

    DragAt position ->
      -- model ! []
      ({model | pos = position}, Cmd.none)

    DragEnd position ->
      -- model ! []
      ({model | pos = position}, Cmd.none)

    MouseMove position ->
      -- model ! []
      ({model | pos = position}, Cmd.none)


valToHour : Int -> Bool -> Int
valToHour val isInner =
  let
    zeroCompensated = if val == 0 then 12 else val
    innerCompensated = if isInner then zeroCompensated + 12 else zeroCompensated
  in
    innerCompensated


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Closed ->
      Sub.none

    HourView ->
      Sub.none --Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

    MinuteView ->
      Sub.none


view : Model -> Html Msg
view model =
  div []
    [ p [] [ text "Clockpicker" ]
    , input
      [ onClick OpenPicker
      , value (formatTime model)
      ]
      []
    , clockPickerWrapper model
    , div [ style [("margin-top", "250px")]] [ text (toString model) ]
    ]


offsetPosition : Json.Decoder Position
offsetPosition =
  Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


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
    [ class "popover clockpicker-popover bottom clockpicker-align-left"
    , style [("display", "block")]
    ]
    [ div [ class "arrow" ] []
    , viewTitle model
    , viewPopoverContentHour model
    , button
      [ class "btn btn-sm btn-default btn-block clockpicker-button"
      , onClick ClosePicker
      ]
      [ text "Done" ]
    ]


viewTitle : Model -> Html Msg
viewTitle model =
  div
    [ class "popover-title" ]
    [ span [ class "clockpicker-span-hours text-primary" ] [ text (formatHourFull model.hour) ]
    , text ":"
    , span [ class "clockpicker-span-minutes" ] [ text (formatMinuteFull model.minute) ]
    ]


drawMinuteView : Model -> Html Msg
drawMinuteView model =
  div
    [ class "popover clockpicker-popover bottom clockpicker-align-left" ]
    [ div [ class "arrow" ] []
    , viewTitle model
    , viewPopoverContentMinute model
    , button
      [ class "btn btn-sm btn-default btn-block clockpicker-button"
      , onClick ClosePicker
      ]
      [ text "Done" ]
    ]


viewPopoverContentMinute : Model -> Html Msg
viewPopoverContentMinute model =
  div
    [ class "popover-content" ]
    [ div [ class "clockpicker-plate" ]
      [ drawMinuteTicks model ]
    , span [ class "clockpicker-am-pm-clock" ] []
    ]


drawMinuteTicks : Model -> Html Msg
drawMinuteTicks model =
  div
    [ class "clockpicker-dial clockpicker-minutes" ]
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
      [ class "clockpicker-tick"
      , style
        [ ("left", (toString left) ++ "px")
        , ("top", (toString top) ++ "px")
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
