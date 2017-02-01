# elm-clockpicker

``` shell
elm package install JordyMoos/elm-clockpicker
```

A reusable clock picker component in Elm.


## Examples

See the [examples][examples] folder and [the ClockPicker demo][demo].

[examples]: https://github.com/JordyMoos/elm-clockpicker/tree/master/examples
[demo]: https://jordymoos.github.io/elm-clockpicker/


## Run examples

- Run `npm install` in the root directory
- Goto `examples/` and run `make` there
- Open `examples/simple/index.html` in your browser


## Settings

```elm
type alias Settings =
    { hourStep : Int
    , minuteStep : Int
    , startTime : StartTime
    , autoClose : Bool
    , twelveHour : Bool
    , doneText : String
    }
```

```elm
import ClockPicker exposing (defaultSettings, StartTime(..))

ClockPicker.init { defaultSettings | minuteStep = 5 }
```

See [ClockPicker.Settings][settings] for detailed information
[settings]: http://package.elm-lang.org/packages/JordyMoos/elm-clockpicker/latest/ClockPicker#settings

## CSS

The CSS for the clock picker is distributed separately. You can grab
the compiled CSS from [here][compiled].

[compiled]: https://github.com/JordyMoos/elm-clockpicker/blob/master/css/elm-clockpicker.css


## Additional copyright notices

Elm ClockPicker is based on the javascript clockpicker from  [weareoutman][weareoutman].
The css and some of the code is translated from his repository.

The structure of this project is inspired by [Bogdanp' elm datepicker][Bogdanp].
The simple example is also inspired from his elm datepicker.

[weareoutman]: https://github.com/weareoutman/clockpicker
[Bogdanp]: https://github.com/Bogdanp/elm-datepicker
