module ClockPicker
    exposing
        ( Msg
        , Settings
        , ClockPicker
        , Time
        , StartTime(..)
        , defaultSettings
        , init
        , update
        , view
        )

{-| A customizable clock picker component.

# ClockPicker
@docs Msg, ClockPicker, Time, StartTime
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
import Time as CoreTime
import Task


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


type alias Hour =
    Int


type alias Minute =
    Int


{-| StartTime
-}
type StartTime
    = EmptyStartTime
    | SetStartTime Hour Minute
    | NowStartTime


{-| The type of clock picker settings.
-}
type alias Settings =
    { hourStep : Int
    , minuteStep : Int
    , startTime : StartTime
    , autoClose : Bool
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
    , startTime = EmptyStartTime
    , autoClose = False
    }


emptyPosition : Position
emptyPosition =
    Position 0 0


{-| The time response record
-}
type alias Time =
    { hour : Hour
    , minute : Minute
    }


emptyTime : Time
emptyTime =
    Time 0 0


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
    | NewTime CoreTime.Time
    | OpenPicker
    | ClosePicker
    | SetHour Int
    | SetMinute Int
    | DragAt Position
    | DragEnd Position
    | MouseMove Position
    | ClickHour
    | ClickMinute
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
    let
        ( startTimeModel, startTimeCmd ) =
            case settings.startTime of
                EmptyStartTime ->
                    ( emptyTime, Cmd.none )

                SetStartTime hour minute ->
                    ( Time hour minute, Cmd.none )

                NowStartTime ->
                    ( emptyTime, Task.perform NewTime CoreTime.now )
    in
        ( ClockPicker <| Model Closed startTimeModel emptyPosition settings
        , startTimeCmd
        )


{-| update
-}
update : Msg -> ClockPicker -> ( ClockPicker, Cmd Msg, Maybe Time )
update msg (ClockPicker ({ state, pos, time, settings } as model)) =
    case msg of
        NoOp ->
            model ! []

        NewTime newTime ->
            let
                hours =
                    (ceiling <| CoreTime.inHours newTime) % 24

                minutes =
                    (floor <| CoreTime.inMinutes newTime) % 60

                time =
                    Time hours minutes
            in
                { model | time = time } ! []

        OpenPicker ->
            { model | state = HourView } ! []

        ClosePicker ->
            { model | state = Closed } ! []

        ClickHour ->
            let
                { value, isInner } =
                    calculateUnitByPosition 12 settings.hourStep True pos

                hour =
                    valToHour value isInner

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


valToHour : Int -> Bool -> Int
valToHour value isInner =
    let
        zeroCompensated =
            if value == 0 then
                12
            else
                value

        innerCompensated =
            if isInner then
                zeroCompensated + 12
            else
                zeroCompensated
    in
        innerCompensated


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


formatTime : Model -> String
formatTime model =
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
        , span [ class "clockpicker-am-pm-clock" ] []
        ]


drawHourCanvas : Model -> Html Msg
drawHourCanvas model =
    drawCanvas ClickHour <| calculateUnitByPosition 12 model.settings.minuteStep True model.pos


drawHourTicks : Model -> Html Msg
drawHourTicks model =
    div
        [ class "clockpicker-dial clockpicker-hours" ]
        (List.map (drawTick (SetHour) (formatHour) 12 1) (List.range 1 24))


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
