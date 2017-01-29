module ClockPicker
    exposing
        ( Msg
        , Settings
        , ClockPicker
        , Time
        , defaultSettings
        , init
        , update
        , view
        )

{-| A customizable clock picker component.

# ClockPicker
@docs Msg, ClockPicker, Time
@docs init, update, view

# Settings
@docs Settings, defaultSettings

-}

import Basics exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (..)
import Json.Decode as Json
import Svg
import Svg.Attributes
import VirtualDom


dialRadius : Float
dialRadius =
    100.0


outerRadius : Float
outerRadius =
    80.0


innerRadius : Float
innerRadius =
    54


tickRadius : Float
tickRadius =
    13.0


dialRadiusString : String
dialRadiusString =
    toString dialRadius


tickRadiusString : String
tickRadiusString =
    toString tickRadius


diameter : Int
diameter =
    round <| dialRadius * 2


type alias Model =
    { state : State
    , hour : Int
    , minute : Int
    , pos : Position
    , settings : Settings
    }


{-| The type of clock picker settings.
-}
type alias Settings =
    { hourStep : Int
    , minuteStep : Int
    }


{-| A record of default settings for the clock picker.
Extend this if you want to customize your clock pciker.


    import ClockPicker exposing (defaultSettings)

    ClockPicker.init { defaultSettings | minuteStep = 5 }

-}
defaultSettings : Settings
defaultSettings =
    { hourStep = 1
    , minuteStep = 1
    }


emptyPosition : Position
emptyPosition =
    Position 0 0


{-| The time response record
-}
type alias Time =
    { hour : Int
    , minute : Int
    }


{-| The ClockPicker model.
-}
type ClockPicker
    = ClockPicker Model


type State
    = HourView
    | MinuteView
    | Closed


{-| An opaque type representing messages that are passed inside the ClockPicker.
-}
type Msg
    = NoOp
    | OpenPicker
    | ClosePicker
    | SetHour Int
    | SetMinute Int
    | DragAt Position
    | DragEnd Position
    | MouseMove Position
    | ClickHour
    | ClickMinute


(!) : Model -> List (Cmd Msg) -> ( ClockPicker, Cmd Msg )
(!) m cs =
    ( ClockPicker m, Cmd.batch cs )


{-| Initialize a ClockPicker given a Settings record.
You must execute the returned command for future purposes


    init
        let
            (clockPicker, clockPickerCmd) = ClockPicker.init defaultSettings
        in
            { picker = clockPicker } ! [ Cmd.map ToClockPicker clockPickerCmd ]

-}
init : Settings -> ( ClockPicker, Cmd Msg )
init settings =
    ( ClockPicker <| Model Closed 0 0 emptyPosition settings
    , Cmd.none
    )


{-| update
-}
update : Msg -> ClockPicker -> ( ClockPicker, Cmd Msg )
update msg (ClockPicker ({ state, pos, hour, minute, settings } as model)) =
    case msg of
        NoOp ->
            model ! []

        OpenPicker ->
            { model | state = HourView } ! []

        ClosePicker ->
            { model | state = Closed } ! []

        ClickHour ->
            let
                x =
                    (toFloat pos.x) - dialRadius

                y =
                    (toFloat pos.y) - dialRadius

                radianTemp =
                    atan2 x (negate y)

                radian =
                    if radianTemp < 0 then
                        pi * 2 + radianTemp
                    else
                        radianTemp

                z =
                    sqrt <| x * x + y * y

                isInner =
                    if z < ((outerRadius + innerRadius) / 2) then
                        True
                    else
                        False

                unit =
                    (toFloat model.settings.hourStep) / 6 * pi

                val =
                    model.settings.hourStep * (round <| radian / unit)

                hour =
                    valToHour val isInner
            in
                { model | hour = hour, state = MinuteView } ! []

        ClickMinute ->
            let
                x =
                    (toFloat pos.x) - dialRadius

                y =
                    (toFloat pos.y) - dialRadius

                radianTemp =
                    atan2 x (negate y)

                radian =
                    if radianTemp < 0 then
                        pi * 2 + radianTemp
                    else
                        radianTemp

                unit =
                    (toFloat model.settings.minuteStep) / 30 * pi

                val =
                    model.settings.minuteStep * (round <| radian / unit)
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
        zeroCompensated =
            if val == 0 then
                12
            else
                val

        innerCompensated =
            if isInner then
                zeroCompensated + 12
            else
                zeroCompensated
    in
        innerCompensated


{-| view
-}
view : ClockPicker -> Html Msg
view (ClockPicker ({ state, pos, hour, minute } as model)) =
    div [ class "clockpicker-container" ]
        [ input
            [ onClick OpenPicker
            , value (formatTime model)
            ]
            []
        , clockPickerWrapper model
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
        , style [ ( "display", "block" ) ]
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
        x =
            (toFloat model.pos.x) - dialRadius

        y =
            (toFloat model.pos.y) - dialRadius

        radianTemp =
            atan2 x (negate y)

        radian =
            if radianTemp < 0 then
                pi * 2 + radianTemp
            else
                radianTemp

        unit =
            (toFloat model.settings.minuteStep) / 30 * pi

        val =
            round <| radian / unit

        radianRounded =
            (toFloat val) * unit

        z =
            sqrt <| x * x + y * y

        radius =
            outerRadius

        cx =
            (sin radianRounded) * radius

        cy =
            negate <| (cos radianRounded) * radius

        cxString =
            toString cx

        cyString =
            toString cy
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
        minute =
            tick * 5

        radius =
            outerRadius

        radian =
            (toFloat tick) / 6 * pi

        left =
            dialRadius + (sin radian) * radius - tickRadius

        top =
            dialRadius - (cos radian) * radius - tickRadius
    in
        div
            [ class "clockpicker-tick"
            , style
                [ ( "left", (toString left) ++ "px" )
                , ( "top", (toString top) ++ "px" )
                ]
            , onClick (SetMinute minute)
            ]
            [ text (formatMinute minute) ]


formatMinute : Int -> String
formatMinute minute =
    case minute of
        60 ->
            "00"

        _ ->
            toString minute


formatMinuteFull : Int -> String
formatMinuteFull minute =
    if minute == 60 then
        "00"
    else if minute < 10 then
        "0" ++ (toString minute)
    else
        toString minute


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
        x =
            (toFloat model.pos.x) - dialRadius

        y =
            (toFloat model.pos.y) - dialRadius

        radianTemp =
            atan2 x (negate y)

        radian =
            if radianTemp < 0 then
                pi * 2 + radianTemp
            else
                radianTemp

        unit =
            (toFloat model.settings.hourStep) / 6 * pi

        val =
            round <| radian / unit

        radianRounded =
            (toFloat val) * unit

        z =
            sqrt <| x * x + y * y

        isInner =
            if z < ((outerRadius + innerRadius) / 2) then
                True
            else
                False

        radius =
            if isInner then
                innerRadius
            else
                outerRadius

        cx =
            (sin radianRounded) * radius

        cy =
            negate <| (cos radianRounded) * radius

        cxString =
            toString cx

        cyString =
            toString cy
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
        radius =
            if tick > 12 then
                innerRadius
            else
                outerRadius

        radian =
            (toFloat tick) / 6 * pi

        left =
            dialRadius + (sin radian) * radius - tickRadius

        top =
            dialRadius - (cos radian) * radius - tickRadius
    in
        div
            [ class "clockpicker-tick"
            , style
                [ ( "left", (toString left) ++ "px" )
                , ( "top", (toString top) ++ "px" )
                ]
            , onClick (SetHour tick)
            ]
            [ text (formatHour tick) ]


formatHour : Int -> String
formatHour hour =
    case hour of
        24 ->
            "00"

        _ ->
            toString hour


formatHourFull : Int -> String
formatHourFull hour =
    if hour == 24 then
        "00"
    else if hour < 10 then
        "0" ++ (toString hour)
    else
        (toString hour)
