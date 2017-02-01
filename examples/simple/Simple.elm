module Simple exposing (main)

import ClockPicker exposing (Time, StartTime(..), defaultSettings)
import Html exposing (Html, div, h1, text)


type Msg
    = ToClockPicker ClockPicker.Msg


type alias Model =
    { time : Maybe Time
    , clockPicker : ClockPicker.ClockPicker
    }


init : ( Model, Cmd Msg )
init =
    let
        ( clockPicker, clockPickerCmd ) =
            ClockPicker.init { defaultSettings | twelveHour = True, startTime = NowStartTime }
    in
        { time = Nothing
        , clockPicker = clockPicker
        }
            ! [ Cmd.map ToClockPicker clockPickerCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ clockPicker } as model) =
    case msg of
        ToClockPicker msg ->
            let
                ( newClockPicker, clockPickerCmd, newTime ) =
                    ClockPicker.update msg clockPicker

                time =
                    case newTime of
                        Nothing ->
                            model.time

                        time ->
                            time
            in
                { model
                    | time = time
                    , clockPicker = newClockPicker
                }
                    ! [ Cmd.map ToClockPicker clockPickerCmd ]


view : Model -> Html Msg
view ({ time, clockPicker } as model) =
    div []
        [ case time of
            Nothing ->
                h1 [] [ text "Pick a time" ]

            Just time ->
                h1 []
                    [ text <|
                        "Selected hour: "
                            ++ toString time.hour
                            ++ " and minute: "
                            ++ toString time.minute
                    ]
        , ClockPicker.view clockPicker
            |> Html.map ToClockPicker
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
