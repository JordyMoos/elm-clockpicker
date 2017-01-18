module Model exposing (..)

import Mouse exposing (..)


dialRadius = 100.0
outerRadius = 80.0

dialRadiusString = toString dialRadius
tickRadiusString = toString tickRadius

innerRadius = 54
tickRadius = 13.0
diameter = round <| dialRadius * 2


type alias Model =
  { state : State
  , hour : Int
  , minute : Int
  , pos : Position
  }


type State
  = HourView
  | MinuteView
  | Closed
