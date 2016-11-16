module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { value : String
  , isVisible : Bool
  }


init : (Model, Cmd Msg)
init =
  (Model "" False, Cmd.none)


type Msg
  = NoOp
  | UpdateValue String
  | OpenPicker
  | ClosePicker


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    OpenPicker ->
      ({model | isVisible = True}, Cmd.none)

    ClosePicker ->
      ({model | isVisible = False}, Cmd.none)

    UpdateValue value ->
      ({model | value = value}, Cmd.none)


view : Model -> Html Msg
view model =
  div
    [ style [ ("background-color", "green") ]
    , onBlur ClosePicker
    ]
    [ p [] [ text "Clockpicker" ]
    , input
      [ onInput UpdateValue
      , onClick OpenPicker
      ]
      []
    , text model.value
    , clockPickerWrapper model
    ]


clockPickerWrapper : Model -> Html Msg
clockPickerWrapper model =
  if model.isVisible then
    drawClockPicker model
  else
    text ""


drawClockPicker : Model -> Html Msg
drawClockPicker model =
  div []
    [ p [] [ text "Fancy clockpicker" ]
    ]

