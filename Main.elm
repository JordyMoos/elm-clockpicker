module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
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
  }


init : (Model, Cmd Msg)
init =
  (Model "", Cmd.none)


type Msg
  = NoOp
  | UpdateValue String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    UpdateValue value ->
      ({model | value = value}, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ p [] [ text "Clockpicker" ]
    , input [ onInput UpdateValue ] []
    , text model.value
    ]
