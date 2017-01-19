module Hour exposing (viewPopoverContentHour, formatHour, formatHourFull)

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
import Msg exposing (..)


offsetPosition : Json.Decoder Position
offsetPosition =
  Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


viewPopoverContentHour : Model -> Html Msg
viewPopoverContentHour model =
  div
    [ class "popover-content" ]
    [ div
        [ class "clockpicker-plate"
        , id "hand-target"
        ]
        [ drawHourTicks model
        , drawHourCanvas model
        ]
    , span [ class "clockpicker-am-pm-clock" ] []
    ]


drawHourCanvas : Model -> Html Msg
drawHourCanvas model =
  let

    x = (toFloat model.pos.x) - dialRadius
    y = (toFloat model.pos.y) - dialRadius

    radianTemp = atan2 x (negate y)
    radian = if radianTemp < 0 then pi * 2 + radianTemp else radianTemp

    unit = hourStep / 6 * pi
    val = round <| radian / unit
    radianRounded = (toFloat val) * unit

    z = sqrt <| x * x + y * y
    isInner = if z < ((outerRadius + innerRadius) / 2) then True else False
    radius = if isInner then innerRadius else outerRadius

    cx = (sin radianRounded) * radius
    cy = negate <| (cos radianRounded) * radius

    cxString = toString cx
    cyString = toString cy
  in
    div
      [ class "clockpicker-canvas"
      , onClick ClickHour
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


drawHourTicks : Model -> Html Msg
drawHourTicks model =
  div
    [ class "clockpicker-dial clockpicker-hours" ]
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
      [ class "clockpicker-tick"
      , style
          [ ("left", (toString left) ++ "px")
          , ("top", (toString top) ++ "px")
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
