module Main exposing (suite)

import Calendar.Tests
import Clock.Tests
import DateTime.Tests
import Test exposing (..)


suite : Test
suite =
    describe "elm-datetime"
        [ Clock.Tests.suite
        , Calendar.Tests.suite
        , DateTime.Tests.suite
        ]
