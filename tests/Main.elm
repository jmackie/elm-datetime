module Main exposing (suite)

import DateTime
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (Test, describe, fuzz, test)
import Time


suite : Test
suite =
    describe "DateTime"
        [ describe "Time.Posix isomorphism"
            [ fuzz (Fuzz.map2 Tuple.pair zoneFuzzer posixFuzzer) "fromPosix >> toPosix == id" <|
                \( randomZone, randomPosix ) ->
                    DateTime.fromPosix randomZone randomPosix
                        |> DateTime.toPosix
                        |> Expect.equal randomPosix
            , fuzz posixFuzzer "fromUtcDateAndTime is lossless" <|
                \randomPosix ->
                    let
                        dateTime =
                            DateTime.fromPosix Time.utc randomPosix
                    in
                    DateTime.fromUtcDateAndTime (DateTime.date dateTime) (DateTime.time dateTime)
                        |> Expect.equal dateTime
            , fuzz posixFuzzer "fromPosix >> fromUtcDateAndTime >> toPosix == id" <|
                \randomPosix ->
                    let
                        dateTime =
                            DateTime.fromPosix Time.utc randomPosix
                    in
                    DateTime.fromUtcDateAndTime (DateTime.date dateTime) (DateTime.time dateTime)
                        |> DateTime.toPosix
                        |> Expect.equal randomPosix
            ]
        ]


posixFuzzer : Fuzzer Time.Posix
posixFuzzer =
    Fuzz.intRange 0 Random.maxInt
        |> Fuzz.map Time.millisToPosix


zoneFuzzer : Fuzzer Time.Zone
zoneFuzzer =
    Fuzz.map2 (\h m -> Time.customZone (h * 60 + m) [])
        (Fuzz.intRange -12 12)
        (Fuzz.oneOf [ Fuzz.constant 0, Fuzz.constant 30 ])
