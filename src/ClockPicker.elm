module ClockPicker exposing
    ( Msg, ClockPicker, Time, StartTime(..)
    , init, update, view
    , Settings, defaultSettings
    )

{-| A customizable clock picker component.

  - 24 hour and 12 hour AM PM support.
  - Configure the hour and minute step sizes.
  - Set the initial time or configure it as now.

See the examples and demo on github.


# ClockPicker

@docs Msg, ClockPicker, Time, StartTime
@docs init, update, view


# Settings

@docs Settings, defaultSettings

-}

import Basics exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Svg
import Svg.Attributes
import Task
import Time as CoreTime
import VirtualDom
import String


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
    String.fromFloat dialRadius


tickRadiusString : String
tickRadiusString =
    String.fromFloat tickRadius


diameter : Int
diameter =
    round <| dialRadius * 2

type alias Position =
    { x : Int
    , y : Int
    }

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


{-| The possible start time options

        import ClockPicker exposing (defaultSettings, StartTime(..))


        ClockPicker.init { defaultSettings | startTime = EmptyStartTime }
        ClockPicker.init { defaultSettings | startTime = SetStartTime 22 30 }
        ClockPicker.init { defaultSettings | startTime = NowStartTime }

`EmptyStartTime` will set the the start time to 00:00
`SetStartTime Hour Minute` let you specify the hour and Minute
`NowStartTime` will set the hour and minute to the current time

-}
type StartTime
    = EmptyStartTime
    | SetStartTime Hour Minute
    | NowStartTime


{-| The type of clock picker settings.

`hourStep` will let you specify the incremental step size op hours.
For example setting hourStep to 2 will only allow even hours to be clicked.

`minuteStep` same as for the `hourStep` but then for the minute.

`startTime` lets you specify the startTime of the ClockPicker.
See `ClockPicker.StartTime` for the options.

`autoClose` Determines if the ClockPicker should close after selecting a minute.

`twelveHour` Configure the ClockPicker to use a 24 view or 12 hour with AM and PM.

`doneText` The text of the done button.

-}
type alias Settings =
    { hourStep : Int
    , minuteStep : Int
    , startTime : StartTime
    , autoClose : Bool
    , twelveHour : Bool
    , doneText : String
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
    , autoClose = True
    , twelveHour = False
    , doneText = "Done"
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
    | NewTime CoreTime.Posix
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



{-| Initialize a ClockPicker given a Settings record.
You must execute the returned command.

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


{-| The clock picker update function. The third value in the returned
tuple represents the picked time, it is `Nothing` if nothing happend
and `Just Time` if the time is updated.
-}
update : Msg -> ClockPicker -> ( ClockPicker, Cmd Msg, Maybe Time )
update msg (ClockPicker ({ state, pos, time, settings } as model)) =
    case msg of
        NoOp ->
            ( ClockPicker model
            , Cmd.none
            , Nothing
            )

        NewTime newTime ->
            let
                hours =
                    CoreTime.toHour CoreTime.utc newTime

                minutes =
                    CoreTime.toMinute  CoreTime.utc newTime

            in
            ( ClockPicker { model | time = Time hours minutes }
            , Cmd.none
            , Nothing
            )

        OpenPicker ->
            ( ClockPicker { model | state = HourView }
            , Cmd.none
            , Nothing
            )

        ClosePicker ->
            ( ClockPicker { model | state = Closed }
            , Cmd.none
            , Nothing
            )

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
            ( ClockPicker { model | pos = position }
            , Cmd.none
            , Nothing
            )

        DragEnd position ->
            ( ClockPicker { model | pos = position }
            , Cmd.none
            , Nothing
            )

        MouseMove position ->
            ( ClockPicker { model | pos = position }
            , Cmd.none
            , Nothing
            )

        ShowHour ->
            ( ClockPicker{ model | state = HourView }
            , Cmd.none
            , Nothing
            )

        ShowMinute ->
            ( ClockPicker { model | state = MinuteView }
            , Cmd.none
            , Nothing
            )


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
            toFloat pos.x - dialRadius

        y =
            toFloat pos.y - dialRadius

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
            allowInner && z * 2 < outerRadius + innerRadius

        unit =
            toFloat steps / toFloat units * pi * 2

        value =
            steps * (round <| radian / unit)

        radius =
            if isInner then
                innerRadius

            else
                outerRadius

        radianRounded =
            toFloat value * unit

        cx =
            sin radianRounded * radius

        cy =
            negate <| cos radianRounded * radius

        cxString =
            String.fromFloat cx

        cyString =
            String.fromFloat cy
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
        formatHourTwelveHourFull model.time.hour ++ ":" ++ formatMinuteFull model.time.minute ++ " " ++ formatAmPm model.time.hour

    else
        formatHourFull model.time.hour ++ ":" ++ formatMinuteFull model.time.minute


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
            toFloat tick / 12 * pi * 2

        left =
            dialRadius + sin radian * radius - tickRadius

        top =
            dialRadius - cos radian * radius - tickRadius

        actualValue =
            tick * visualStepSize
    in
    div
        [ class "clockpicker-tick"
        , style "left" (String.fromFloat left ++ "px")
        , style "top" (String.fromFloat top ++ "px")
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
                , VirtualDom.on "mousemove" <| VirtualDom.Normal (Json.map MouseMove offsetPosition)
                , Svg.Attributes.fillOpacity "0"
                ]
                []
            ]
        ]


drawHourView : Model -> Html Msg
drawHourView model =
    div
        [ class "popover clockpicker-popover bottom clockpicker-align-left"
        , style "display" "block"
        ]
        [ div [ class "arrow" ] []
        , viewTitle model
        , viewPopoverContentHour model
        , button
            [ class "btn btn-sm btn-default btn-block clockpicker-button"
            , onClick ClosePicker
            ]
            [ text model.settings.doneText ]
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
            [ class <| "clockpicker-span-hours" ++ isActive HourView
            , onClick ShowHour
            ]
            [ text (formatHourFull model.time.hour) ]
        , text ":"
        , span
            [ class <| "clockpicker-span-minutes" ++ isActive MinuteView
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
            [ class <| "clockpicker-span-hours" ++ isActive HourView
            , onClick ShowHour
            ]
            [ text (formatHourTwelveHourFull model.time.hour) ]
        , text ":"
        , span
            [ class <| "clockpicker-span-minutes" ++ isActive MinuteView
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
        (List.map (drawTick SetMinute formatMinute 60 5) (List.range 1 (60 // 5)))


formatMinute : Int -> String
formatMinute minute =
    case minute of
        60 ->
            "00"

        _ ->
            String.fromInt minute


formatMinuteFull : Int -> String
formatMinuteFull minute =
    if minute == 60 then
        "00"

    else if minute < 10 then
        "0" ++ String.fromInt minute

    else
        String.fromInt minute


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
        (List.map (drawTick SetHour formatHour 12 1) (List.range 1 rangeMax))


formatHour : Int -> String
formatHour hour =
    case hour of
        24 ->
            "00"

        _ ->
            String.fromInt hour


formatHourFull : Int -> String
formatHourFull hour =
    if hour == 24 then
        "00"

    else if hour < 10 then
        "0" ++ String.fromInt hour

    else
        String.fromInt hour


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
