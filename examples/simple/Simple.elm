module Simple exposing (main)

import ClockPicker exposing (StartTime(..), Time, defaultSettings)
import Html exposing (Html, div, h1, text)
import Browser
import String


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
            ClockPicker.init { defaultSettings | startTime = NowStartTime }
    in
    ( { time = Nothing
      , clockPicker = clockPicker
      }
    , Cmd.map ToClockPicker clockPickerCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ clockPicker } as model) =
    case msg of
        ToClockPicker innerMsg ->
            let
                ( newClockPicker, clockPickerCmd, newTime ) =
                    ClockPicker.update innerMsg clockPicker

                time =
                    case newTime of
                        Nothing ->
                            model.time

                        _ ->
                            newTime
            in
            ( { model
                | time = time
                , clockPicker = newClockPicker
              }
            , Cmd.map ToClockPicker clockPickerCmd
            )


view : Model -> Html Msg
view ({ time, clockPicker } as model) =
    div []
        [ case time of
            Nothing ->
                h1 [] [ text "Pick a time" ]

            Just {hour, minute} ->
                h1 []
                    [ text <|
                        "Selected hour: "
                            ++ String.fromInt hour
                            ++ " and minute: "
                            ++ String.fromInt minute
                    ]
        , ClockPicker.view clockPicker
            |> Html.map ToClockPicker
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
