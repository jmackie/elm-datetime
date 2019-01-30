module DateTime.Tests exposing (suite)

import DateTime
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (Test)
import Time


suite : Test
suite =
    Test.describe "DateTime"
        [ isoTest
        ]


fuzzOptions : Test.FuzzOptions
fuzzOptions =
    { runs = 1000 }


isoTest : Test
isoTest =
    Test.describe "fromPosix <-> toPosix"
        [ Test.fuzzWith fuzzOptions posixFuzzer "isomorphism" <|
            \posix ->
                Expect.equal posix (DateTime.toPosix (DateTime.fromPosix posix))
        ]



-- FUZZERS --


posixFuzzer : Fuzzer Time.Posix
posixFuzzer =
    Fuzz.intRange 0 Random.maxInt |> Fuzz.map Time.millisToPosix
