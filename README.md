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

| Name | Default | Description |
| ---- | ------- | ----------- |
| startTime | EmptyStartTime | Set the initial time of the clockpicker input.<br>Possible values are:<br>`EmptyStartTime` (Set time to 00:00)<br>`SetStartTime Hour Minute` (Sets the to the given hour and minute)<br>`NowStartTime` (Set the time to the current time)<br>See the `ClockPicker.StartTime` type for more information |
| hourStep | 1 | Set the incremental step for hours |
| minuteStep | 1 | Set the incremental step for minutes |
| autoClose | True | Should the ClockPicker close after selecting the minute |
| twelveHour | False | Use twelve hour style clock with AM and PM |
| doneText | "Done" | Set the text for the done button |

See `ClockPicker.Settings` for more information


## CSS

The CSS for the clock picker is distributed separately.  You can grab
the compiled CSS from [here][compiled].

[compiled]: https://github.com/JordyMoos/elm-clockpicker/blob/master/css/elm-clockpicker.css


## Additional copyright notices

Elm ClockPicker is based on the javascript clockpicker from  [weareoutman][weareoutman].
The css and some of the code is translated from his repository.

The structure of this project is inspired by [Bogdanp' elm datepicker][Bogdanp].
The simple example is also inspired from his elm datepicker.

[weareoutman]: https://github.com/weareoutman/clockpicker
[Bogdanp]: https://github.com/Bogdanp/elm-datepicker
