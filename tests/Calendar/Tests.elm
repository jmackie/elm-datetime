module Calendar.Tests exposing (suite)

import Array
import Calendar
import Calendar.Internal as Internal
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (Test)
import Time


suite : Test
suite =
    Test.describe "Calendar"
        [ fromPosixTest
        , fromRawTest
        , compareTest
        ]


fuzzOptions : Test.FuzzOptions
fuzzOptions =
    { runs = 1000 }


fromPosixTest : Test
fromPosixTest =
    Test.describe "fromPosix"
        [ Test.fuzzWith fuzzOptions posixFuzzer "returns the correct year" <|
            \posix ->
                Expect.equal
                    (Time.toYear Time.utc posix)
                    (Calendar.year <| Calendar.fromPosix posix)
        , Test.fuzzWith fuzzOptions posixFuzzer "returns the correct month" <|
            \posix ->
                Expect.equal
                    (Time.toMonth Time.utc posix)
                    (Calendar.month <| Calendar.fromPosix posix)
        , Test.fuzzWith fuzzOptions posixFuzzer "returns the correct day" <|
            \posix ->
                Expect.equal
                    (Time.toDay Time.utc posix)
                    (Calendar.day <| Calendar.fromPosix posix)
        ]


fromRawTest : Test
fromRawTest =
    Test.describe "fromRaw"
        [ Test.fuzzWith fuzzOptions validRawFuzzer "preserves year" <|
            \raw ->
                Expect.equal
                    (Just raw.year)
                    (Maybe.map Calendar.year <| Calendar.fromRaw raw)
        , Test.fuzzWith fuzzOptions validRawFuzzer "preserves month" <|
            \raw ->
                Expect.equal
                    (Just raw.month)
                    (Maybe.map (Calendar.month >> Internal.monthToInt) <| Calendar.fromRaw raw)
        , Test.fuzzWith fuzzOptions validRawFuzzer "preserves day" <|
            \raw ->
                Expect.equal
                    (Just raw.day)
                    (Maybe.map Calendar.day <| Calendar.fromRaw raw)
        , Test.fuzzWith fuzzOptions invalidRawFuzzer "rejects invalid dates" <|
            \raw ->
                Expect.equal Nothing (Calendar.fromRaw raw)
        ]


compareTest : Test
compareTest =
    Test.describe "compare"
        [ Test.fuzzWith fuzzOptions (Fuzz.tuple ( validRawFuzzer, validRawFuzzer )) "compares correctly" <|
            \( a, b ) ->
                Expect.equal
                    (Just <| compareRaw a b)
                    (Maybe.map2 Calendar.compare (Calendar.fromRaw a) (Calendar.fromRaw b))
        ]


compareRaw : Internal.Raw -> Internal.Raw -> Order
compareRaw a b =
    case compare a.year b.year of
        EQ ->
            case compare a.month b.month of
                EQ ->
                    compare a.day b.day

                order ->
                    order

        order ->
            order



-- FUZZERS --


posixFuzzer : Fuzzer Time.Posix
posixFuzzer =
    Fuzz.intRange 0 Random.maxInt |> Fuzz.map Time.millisToPosix


monthFuzzer : Fuzzer Time.Month
monthFuzzer =
    Fuzz.oneOf (List.map Fuzz.constant (Array.toList Internal.allTheMonths))


validRawFuzzer : Fuzzer Internal.Raw
validRawFuzzer =
    Fuzz.map3 Internal.Raw
        (Fuzz.intRange 1 Random.maxInt)
        (Fuzz.intRange 1 12)
        (Fuzz.intRange 1 28)


invalidRawFuzzer : Fuzzer Internal.Raw
invalidRawFuzzer =
    Fuzz.map3 Internal.Raw
        (Fuzz.intRange Random.minInt 0)
        (fuzzOutsideIntRange 1 12)
        (fuzzOutsideIntRange 1 31)


dateFuzzer : Fuzzer Calendar.Date
dateFuzzer =
    Fuzz.map3
        (\year month day ->
            Internal.Date
                { year = Internal.Year year
                , month = month
                , day = Internal.Day day
                }
        )
        (Fuzz.intRange 1 Random.maxInt)
        monthFuzzer
        (Fuzz.intRange 1 28)


fuzzOutsideIntRange : Int -> Int -> Fuzzer Int
fuzzOutsideIntRange lower upper =
    Fuzz.oneOf
        [ Fuzz.intRange Random.minInt (lower - 1)
        , Fuzz.intRange (upper + 1) Random.maxInt
        ]
