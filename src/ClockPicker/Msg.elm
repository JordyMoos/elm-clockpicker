module ClockPicker.Msg exposing (..)

import Mouse exposing (..)


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
