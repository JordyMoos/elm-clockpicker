module ClockPicker.Minute exposing (viewPopoverContentMinute, formatMinute, formatMinuteFull)

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

import ClockPicker.Model exposing (..)
import ClockPicker.Msg exposing (..)


offsetPosition : Json.Decoder Position
offsetPosition =
  Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


viewPopoverContentMinute : Model -> Html Msg
viewPopoverContentMinute model =
  div
    [ class "popover-content" ]
    [ div
        [ class "clockpicker-plate"
        , id "hand-target"
        ]
        [ drawMinuteTicks model
        , drawMinuteCanvas model
        ]
    , span [ class "clockpicker-am-pm-clock" ] []
    ]


drawMinuteCanvas : Model -> Html Msg
drawMinuteCanvas model =
  let

    x = (toFloat model.pos.x) - dialRadius
    y = (toFloat model.pos.y) - dialRadius

    radianTemp = atan2 x (negate y)
    radian = if radianTemp < 0 then pi * 2 + radianTemp else radianTemp

    unit = minuteStep / 30 * pi
    val = round <| radian / unit
    radianRounded = (toFloat val) * unit

    z = sqrt <| x * x + y * y
    radius = outerRadius

    cx = (sin radianRounded) * radius
    cy = negate <| (cos radianRounded) * radius

    cxString = toString cx
    cyString = toString cy
  in
    div
      [ class "clockpicker-canvas"
      , onClick ClickMinute
      ]
      [ Svg.svg
        [ width diameter
        , height diameter
        ]
        [ Svg.g
          [ Svg.Attributes.transform <| "translate(" ++ dialRadiusString ++ "," ++ dialRadiusString ++ ")"
          ]
          [ Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 cxString
            , Svg.Attributes.y2 cyString
            ]
            []
          , Svg.circle
            [ Svg.Attributes.class "clockpicker-canvas-fg"
            , Svg.Attributes.r "3.5"
            , Svg.Attributes.cx cxString
            , Svg.Attributes.cy cyString
            ]
            []
          , Svg.circle
            [ Svg.Attributes.class "clockpicker-canvas-bg"
            , Svg.Attributes.r tickRadiusString
            , Svg.Attributes.cx cxString
            , Svg.Attributes.cy cyString
            , Svg.Attributes.fillOpacity "0.5"
            ]
            []
          , Svg.circle
            [ Svg.Attributes.class "clockpicker-canvas-bearing"
            , Svg.Attributes.r "2"
            , Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            ]
            []
          ]
        , Svg.rect
          [ width diameter
          , height diameter
          , VirtualDom.on "mousemove" (Json.map MouseMove offsetPosition)
          , Svg.Attributes.fillOpacity "0"
          ]
          []
        ]
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
