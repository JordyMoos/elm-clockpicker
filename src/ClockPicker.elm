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
    , time : Time
    , pos : Position
    , settings : Settings
    }


{-| The type of clock picker settings.
-}
type alias Settings =
    { hourStep : Int
    , minuteStep : Int
    , autoClose : Bool
    , twelveHour : Bool
    }


type alias FromPositionResult =
    { value : Int
    , isInner : Bool
    , cxString : String
    , cyString : String
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
    , autoClose = False
    , twelveHour = False
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


defaultTime : Time
defaultTime =
    Time 12 0


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
    | ClickAm
    | ClickPm
    | ShowHour
    | ShowMinute


(!) : Model -> List (Cmd Msg) -> ( ClockPicker, Cmd Msg, Maybe Time )
(!) model cmds =
    ( ClockPicker model, Cmd.batch cmds, Nothing )


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
    ( ClockPicker <| Model Closed defaultTime emptyPosition settings
    , Cmd.none
    )


{-| update
-}
update : Msg -> ClockPicker -> ( ClockPicker, Cmd Msg, Maybe Time )
update msg (ClockPicker ({ state, pos, time, settings } as model)) =
    case msg of
        NoOp ->
            model ! []

        OpenPicker ->
            { model | state = HourView } ! []

        ClosePicker ->
            { model | state = Closed } ! []

        ClickHour ->
            let
                allowInner =
                    not settings.twelveHour

                { value, isInner } =
                    calculateUnitByPosition 12 settings.hourStep allowInner pos

                hour =
                    valToHour value isInner time.hour settings.twelveHour

                newTime =
                    { time | hour = hour }
            in
                ( ClockPicker { model | time = newTime, state = MinuteView }
                , Cmd.none
                , Just newTime
                )

        ClickMinute ->
            let
                { value } =
                    calculateUnitByPosition 60 settings.minuteStep False pos

                newTime =
                    { time | minute = value }

                newState =
                    if settings.autoClose then
                        Closed
                    else
                        MinuteView
            in
                ( ClockPicker { model | time = newTime, state = newState }
                , Cmd.none
                , Just newTime
                )

        ClickAm ->
            let
                newHour =
                    if time.hour > 12 then
                        time.hour - 12
                    else
                        time.hour

                newTime =
                    { time | hour = newHour }
            in
                ( ClockPicker { model | time = newTime }
                , Cmd.none
                , Just newTime
                )

        ClickPm ->
            let
                newHour =
                    if time.hour <= 12 then
                        time.hour + 12
                    else
                        time.hour

                newTime =
                    { time | hour = newHour }
            in
                ( ClockPicker { model | time = newTime }
                , Cmd.none
                , Just newTime
                )

        SetHour hour ->
            let
                newTime =
                    { time | hour = hour }
            in
                ( ClockPicker { model | time = newTime, state = MinuteView }
                , Cmd.none
                , Just newTime
                )

        SetMinute minute ->
            let
                newTime =
                    { time | minute = minute }
            in
                ( ClockPicker { model | time = newTime, state = Closed }
                , Cmd.none
                , Just newTime
                )

        DragAt position ->
            { model | pos = position } ! []

        DragEnd position ->
            { model | pos = position } ! []

        MouseMove position ->
            { model | pos = position } ! []

        ShowHour ->
            { model | state = HourView } ! []

        ShowMinute ->
            { model | state = MinuteView } ! []


{-| view
-}
view : ClockPicker -> Html Msg
view (ClockPicker ({ state, pos, time, settings } as model)) =
    div [ class "clockpicker-container" ]
        [ input
            [ onClick OpenPicker
            , value (formatTime model)
            ]
            []
        , clockPickerWrapper model
        ]


calculateUnitByPosition : Int -> Int -> Bool -> Position -> FromPositionResult
calculateUnitByPosition units steps allowInner pos =
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
            if allowInner && z < ((outerRadius + innerRadius) / 2) then
                True
            else
                False

        unit =
            (toFloat steps) / (toFloat units) * pi * 2

        value =
            steps * (round <| radian / unit)

        radius =
            if isInner then
                innerRadius
            else
                outerRadius

        radianRounded =
            (toFloat value) * unit

        cx =
            (sin radianRounded) * radius

        cy =
            negate <| (cos radianRounded) * radius

        cxString =
            toString cx

        cyString =
            toString cy
    in
        FromPositionResult value isInner cxString cyString


valToHour : Int -> Bool -> Int -> Bool -> Int
valToHour value isInner previousHour twelveHour =
    let
        zeroCompensated x =
            if x == 0 then
                12
            else
                x

        innerCompensated x =
            if isInner then
                x + 12
            else
                x

        twelveHourCompensated x =
            if twelveHour && previousHour > 12 then
                x + 12
            else
                x
    in
        twelveHourCompensated << innerCompensated << zeroCompensated <| value


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


formatTime : Model -> String
formatTime model =
    if model.settings.twelveHour then
        (formatHourTwelveHourFull model.time.hour) ++ ":" ++ (formatMinuteFull model.time.minute) ++ " " ++ (formatAmPm model.time.hour)
    else
        (formatHourFull model.time.hour) ++ ":" ++ (formatMinuteFull model.time.minute)


clockPickerWrapper : Model -> Html Msg
clockPickerWrapper model =
    case model.state of
        Closed ->
            text ""

        HourView ->
            drawHourView model

        MinuteView ->
            drawMinuteView model


drawTick : (Int -> Msg) -> (Int -> String) -> Int -> Int -> Int -> Html Msg
drawTick onClickMsg formatter outerRadiusMax visualStepSize tick =
    let
        radius =
            if tick > outerRadiusMax then
                innerRadius
            else
                outerRadius

        radian =
            (toFloat tick) / 12 * pi * 2

        left =
            dialRadius + (sin radian) * radius - tickRadius

        top =
            dialRadius - (cos radian) * radius - tickRadius

        actualValue =
            tick * visualStepSize
    in
        div
            [ class "clockpicker-tick"
            , style
                [ ( "left", (toString left) ++ "px" )
                , ( "top", (toString top) ++ "px" )
                ]
            , onClick (onClickMsg actualValue)
            ]
            [ text (formatter actualValue) ]


drawCanvas : Msg -> FromPositionResult -> Html Msg
drawCanvas onClickMsg result =
    div
        [ class "clockpicker-canvas"
        , onClick onClickMsg
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
                    , Svg.Attributes.x2 result.cxString
                    , Svg.Attributes.y2 result.cyString
                    ]
                    []
                , Svg.circle
                    [ Svg.Attributes.class "clockpicker-canvas-fg"
                    , Svg.Attributes.r "3.5"
                    , Svg.Attributes.cx result.cxString
                    , Svg.Attributes.cy result.cyString
                    ]
                    []
                , Svg.circle
                    [ Svg.Attributes.class "clockpicker-canvas-bg"
                    , Svg.Attributes.r tickRadiusString
                    , Svg.Attributes.cx result.cxString
                    , Svg.Attributes.cy result.cyString
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
    if model.settings.twelveHour then
        viewTitleTwelveHour model
    else
        viewTitleTwentyFourHour model


viewTitleTwentyFourHour : Model -> Html Msg
viewTitleTwentyFourHour model =
    let
        isActive state =
            if state == model.state then
                " text-primary"
            else
                ""
    in
        div
            [ class "popover-title" ]
            [ span
                [ class <| "clockpicker-span-hours" ++ (isActive HourView)
                , onClick ShowHour
                ]
                [ text (formatHourFull model.time.hour) ]
            , text ":"
            , span
                [ class <| "clockpicker-span-minutes" ++ (isActive MinuteView)
                , onClick ShowMinute
                ]
                [ text (formatMinuteFull model.time.minute) ]
            ]


viewTitleTwelveHour : Model -> Html Msg
viewTitleTwelveHour model =
    let
        isActive state =
            if state == model.state then
                " text-primary"
            else
                ""

        toggleAmPm =
            if model.time.hour > 12 then
                ClickAm
            else
                ClickPm
    in
        div
            [ class "popover-title" ]
            [ span
                [ class <| "clockpicker-span-hours" ++ (isActive HourView)
                , onClick ShowHour
                ]
                [ text (formatHourTwelveHourFull model.time.hour) ]
            , text ":"
            , span
                [ class <| "clockpicker-span-minutes" ++ (isActive MinuteView)
                , onClick ShowMinute
                ]
                [ text (formatMinuteFull model.time.minute) ]
            , text " "
            , span
                [ class <| "clockpicker-span-am-pm"
                , onClick toggleAmPm
                ]
                [ text (formatAmPm model.time.hour) ]
            ]


viewAmPm : Model -> Html Msg
viewAmPm model =
    if model.settings.twelveHour then
        span
            [ class "clockpicker-am-pm-clock" ]
            [ button
                [ class "btn btn-sm btn-default clockpicker-button am-button"
                , onClick ClickAm
                ]
                [ text "AM" ]
            , button
                [ class "btn btn-sm btn-default clockpicker-button pm-button"
                , onClick ClickPm
                ]
                [ text "PM" ]
            ]
    else
        text ""


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
        , viewAmPm model
        , span [ class "clockpicker-am-pm-clock" ] []
        ]


drawMinuteCanvas : Model -> Html Msg
drawMinuteCanvas model =
    drawCanvas ClickMinute <| calculateUnitByPosition 60 model.settings.minuteStep False model.pos


drawMinuteTicks : Model -> Html Msg
drawMinuteTicks model =
    div
        [ class "clockpicker-dial clockpicker-minutes" ]
        (List.map (drawTick (SetMinute) (formatMinute) 60 5) (List.range 1 (60 // 5)))


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
        , viewAmPm model
        ]


drawHourCanvas : Model -> Html Msg
drawHourCanvas model =
    let
        allowInner =
            not model.settings.twelveHour
    in
        drawCanvas ClickHour <| calculateUnitByPosition 12 model.settings.minuteStep allowInner model.pos


drawHourTicks : Model -> Html Msg
drawHourTicks model =
    let
        rangeMax =
            if model.settings.twelveHour then
                12
            else
                24
    in
        div
            [ class "clockpicker-dial clockpicker-hours" ]
            (List.map (drawTick (SetHour) (formatHour) 12 1) (List.range 1 rangeMax))


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


formatHourTwelveHourFull : Int -> String
formatHourTwelveHourFull hour =
    if hour == 0 then
        "12"
    else if hour > 12 then
        formatHourFull (hour - 12)
    else
        formatHourFull hour


formatAmPm : Int -> String
formatAmPm hour =
    if hour > 12 then
        "PM"
    else
        "AM"
