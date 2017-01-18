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
import Minute exposing (..)


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
      model ! []

    OpenPicker ->
      { model | state = HourView } ! []

    ClosePicker ->
      { model | state = Closed } ! []

    ClickHour ->
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
        { model | hour = hour, state = MinuteView } ! []

    ClickMinute ->
      let
        x = (toFloat model.pos.x) - dialRadius
        y = (toFloat model.pos.y) - dialRadius

        radianTemp = atan2 x (negate y)
        radian = if radianTemp < 0 then pi * 2 + radianTemp else radianTemp

        unit = 1 / 30 * pi
        val = round <| radian / unit
      in
        { model | minute = val, state = Closed } ! []

    SetHour hour ->
      { model | hour = hour, state = MinuteView } ! []

    SetMinute minute ->
      { model | minute = minute, state = Closed } ! []

    DragAt position ->
      { model | pos = position } ! []

    DragEnd position ->
      { model | pos = position } ! []

    MouseMove position ->
      { model | pos = position } ! []


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
    [ span
        [ class "clockpicker-span-hours text-primary" ]
        [ text (formatHourFull model.hour) ]
    , text ":"
    , span
        [ class "clockpicker-span-minutes" ]
        [ text (formatMinuteFull model.minute) ]
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
