module Main exposing (..)

import Html exposing (..)
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
  { id : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1, Cmd.none)


type Msg
  = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ text "Clockpicker" ]
